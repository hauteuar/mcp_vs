#!/usr/bin/env python3
"""
Mainframe Codebase Analysis MCP Server
Handles parsing, embeddings, semantic search, and DB2/VSAM operations
"""

import os
import json
import sqlite3
import pandas as pd
import numpy as np
import asyncio
import logging
from pathlib import Path
from typing import List, Dict, Any, Optional, Tuple
from dataclasses import dataclass
import re
import zipfile
import csv

# MCP imports (install with: pip install mcp)
from mcp.server.models import InitializationOptions
from mcp.server import NotificationOptions, Server
from mcp.server.models import (
    CallToolRequest,
    CallToolResult,
    TextContent,
    Tool,
    INVALID_PARAMS,
)
from mcp.types import JSONRPCMessage

# Embedding and search imports
try:
    from sentence_transformers import SentenceTransformer
    import faiss
    HAS_EMBEDDINGS = True
except ImportError:
    HAS_EMBEDDINGS = False
    print("Warning: sentence-transformers or faiss not installed. Embedding features disabled.")

# ANTLR4 imports (install with: pip install antlr4-python3-runtime)
try:
    from antlr4 import *
    HAS_ANTLR = True
except ImportError:
    HAS_ANTLR = False
    print("Warning: ANTLR4 not installed. Code parsing features limited.")

# DB2 connection (install ibm_db if available)
try:
    import ibm_db
    import ibm_db_dbi
    HAS_DB2 = True
except ImportError:
    HAS_DB2 = False
    print("Warning: IBM DB2 driver not installed. Using mock DB2 data.")

logging.basicConfig(level=logging.INFO)
logger = logging.getLogger(__name__)

@dataclass
class CodebaseMetadata:
    """Structured metadata for mainframe artifacts"""
    programs: Dict[str, Dict] = None
    copybooks: Dict[str, Dict] = None
    db2_tables: Dict[str, Dict] = None
    vsam_files: Dict[str, Dict] = None
    friendly_names: Dict[str, str] = None
    
    def __post_init__(self):
        if self.programs is None:
            self.programs = {}
        if self.copybooks is None:
            self.copybooks = {}
        if self.db2_tables is None:
            self.db2_tables = {}
        if self.vsam_files is None:
            self.vsam_files = {}
        if self.friendly_names is None:
            self.friendly_names = {}

class MainframeParser:
    """ANTLR4-based parser for mainframe artifacts"""
    
    def __init__(self):
        self.metadata = CodebaseMetadata()
    
    def parse_cobol_file(self, file_path: str) -> Dict[str, Any]:
        """Parse COBOL file and extract metadata"""
        try:
            with open(file_path, 'r', encoding='utf-8', errors='ignore') as f:
                content = f.read().upper()
            
            # Extract PROGRAM-ID
            program_id_match = re.search(r'PROGRAM-ID\s*\.\s*(\w+)', content)
            program_id = program_id_match.group(1) if program_id_match else Path(file_path).stem
            
            # Extract COPY statements
            copy_matches = re.findall(r'COPY\s+(\w+)', content)
            
            # Extract DB2 table references
            db2_matches = re.findall(r'FROM\s+(\w+)', content)
            db2_matches.extend(re.findall(r'UPDATE\s+(\w+)', content))
            db2_matches.extend(re.findall(r'INSERT\s+INTO\s+(\w+)', content))
            
            # Extract VSAM file references
            vsam_matches = re.findall(r'SELECT\s+(\w+)', content)
            
            return {
                'program_id': program_id,
                'file_path': file_path,
                'copybooks': list(set(copy_matches)),
                'db2_tables': list(set(db2_matches)),
                'vsam_files': list(set(vsam_matches)),
                'line_count': content.count('\n')
            }
        except Exception as e:
            logger.error(f"Error parsing COBOL file {file_path}: {e}")
            return {'program_id': Path(file_path).stem, 'file_path': file_path, 'error': str(e)}
    
    def parse_copybook_file(self, file_path: str) -> Dict[str, Any]:
        """Parse copybook file and extract field structures"""
        try:
            with open(file_path, 'r', encoding='utf-8', errors='ignore') as f:
                content = f.read().upper()
            
            # Extract field definitions (01, 05 levels)
            field_matches = re.findall(r'^\s*(01|05)\s+(\w+)', content, re.MULTILINE)
            
            fields = []
            for level, field_name in field_matches:
                fields.append({'level': level, 'name': field_name})
            
            return {
                'copybook_name': Path(file_path).stem,
                'file_path': file_path,
                'fields': fields,
                'line_count': content.count('\n')
            }
        except Exception as e:
            logger.error(f"Error parsing copybook {file_path}: {e}")
            return {'copybook_name': Path(file_path).stem, 'file_path': file_path, 'error': str(e)}
    
    def parse_jcl_file(self, file_path: str) -> Dict[str, Any]:
        """Parse JCL file and extract job information"""
        try:
            with open(file_path, 'r', encoding='utf-8', errors='ignore') as f:
                content = f.read().upper()
            
            # Extract JOB name
            job_match = re.search(r'//(\w+)\s+JOB', content)
            job_name = job_match.group(1) if job_match else Path(file_path).stem
            
            # Extract EXEC steps
            exec_matches = re.findall(r'//(\w+)\s+EXEC\s+(\w+)', content)
            
            # Extract DD datasets
            dd_matches = re.findall(r'//(\w+)\s+DD\s+DSN=(\w+)', content)
            
            return {
                'job_name': job_name,
                'file_path': file_path,
                'exec_steps': [{'step': step, 'program': prog} for step, prog in exec_matches],
                'datasets': [{'dd_name': dd, 'dataset': dsn} for dd, dsn in dd_matches],
                'line_count': content.count('\n')
            }
        except Exception as e:
            logger.error(f"Error parsing JCL file {file_path}: {e}")
            return {'job_name': Path(file_path).stem, 'file_path': file_path, 'error': str(e)}

class EmbeddingManager:
    """Manages local embeddings for semantic search"""
    
    def __init__(self, model_name: str = "all-MiniLM-L6-v2"):
        self.model = None
        self.index = None
        self.chunks = []
        self.model_name = model_name
        
        if HAS_EMBEDDINGS:
            try:
                self.model = SentenceTransformer(model_name)
                logger.info(f"Loaded embedding model: {model_name}")
            except Exception as e:
                logger.error(f"Failed to load embedding model: {e}")
    
    def chunk_text(self, text: str, max_tokens: int = 800) -> List[str]:
        """Split text into chunks for embedding"""
        # Simple chunking by lines (could be improved with token counting)
        lines = text.split('\n')
        chunks = []
        current_chunk = []
        current_size = 0
        
        for line in lines:
            line_size = len(line.split())
            if current_size + line_size > max_tokens and current_chunk:
                chunks.append('\n'.join(current_chunk))
                current_chunk = [line]
                current_size = line_size
            else:
                current_chunk.append(line)
                current_size += line_size
        
        if current_chunk:
            chunks.append('\n'.join(current_chunk))
        
        return chunks
    
    def embed_codebase(self, files: List[str]) -> bool:
        """Generate embeddings for all files"""
        if not self.model:
            logger.warning("No embedding model available")
            return False
        
        all_chunks = []
        chunk_metadata = []
        
        for file_path in files:
            try:
                with open(file_path, 'r', encoding='utf-8', errors='ignore') as f:
                    content = f.read()
                
                chunks = self.chunk_text(content)
                for i, chunk in enumerate(chunks):
                    all_chunks.append(chunk)
                    chunk_metadata.append({
                        'file_path': file_path,
                        'chunk_id': i,
                        'friendly_name': self._get_friendly_name(file_path)
                    })
            except Exception as e:
                logger.error(f"Error reading file {file_path}: {e}")
        
        if all_chunks:
            embeddings = self.model.encode(all_chunks)
            
            # Create FAISS index
            dimension = embeddings.shape[1]
            self.index = faiss.IndexFlatIP(dimension)  # Inner product for similarity
            self.index.add(embeddings.astype('float32'))
            
            self.chunks = chunk_metadata
            logger.info(f"Created embeddings for {len(all_chunks)} chunks")
            return True
        
        return False
    
    def search_embeddings(self, query: str, top_k: int = 5) -> List[Dict[str, Any]]:
        """Search embeddings for relevant chunks"""
        if not self.model or not self.index:
            return []
        
        query_embedding = self.model.encode([query])
        scores, indices = self.index.search(query_embedding.astype('float32'), top_k)
        
        results = []
        for score, idx in zip(scores[0], indices[0]):
            if idx < len(self.chunks):
                result = self.chunks[idx].copy()
                result['similarity_score'] = float(score)
                results.append(result)
        
        return results
    
    def _get_friendly_name(self, file_path: str) -> str:
        """Generate friendly name for file"""
        return Path(file_path).stem

class DatabaseManager:
    """Manages SQLite metadata and DB2 connections"""
    
    def __init__(self, db_path: str = "mainframe_analysis.db"):
        self.db_path = db_path
        self.init_database()
        self.db2_conn = None
        
        if HAS_DB2:
            self.setup_db2_connection()
    
    def init_database(self):
        """Initialize SQLite database with required tables"""
        conn = sqlite3.connect(self.db_path)
        cursor = conn.cursor()
        
        # Programs table
        cursor.execute('''
            CREATE TABLE IF NOT EXISTS programs (
                id INTEGER PRIMARY KEY,
                program_id TEXT UNIQUE,
                file_path TEXT,
                friendly_name TEXT,
                copybooks TEXT,
                db2_tables TEXT,
                vsam_files TEXT,
                line_count INTEGER,
                created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP
            )
        ''')
        
        # Copybooks table
        cursor.execute('''
            CREATE TABLE IF NOT EXISTS copybooks (
                id INTEGER PRIMARY KEY,
                copybook_name TEXT UNIQUE,
                file_path TEXT,
                friendly_name TEXT,
                fields TEXT,
                line_count INTEGER,
                created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP
            )
        ''')
        
        # DB2 tables metadata
        cursor.execute('''
            CREATE TABLE IF NOT EXISTS db2_tables (
                id INTEGER PRIMARY KEY,
                table_name TEXT UNIQUE,
                friendly_name TEXT,
                columns TEXT,
                primary_keys TEXT,
                created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP
            )
        ''')
        
        # VSAM files metadata
        cursor.execute('''
            CREATE TABLE IF NOT EXISTS vsam_files (
                id INTEGER PRIMARY KEY,
                file_name TEXT UNIQUE,
                file_path TEXT,
                friendly_name TEXT,
                fields TEXT,
                created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP
            )
        ''')
        
        # VSAM to DB2 mapping
        cursor.execute('''
            CREATE TABLE IF NOT EXISTS vsam_db2_mapping (
                id INTEGER PRIMARY KEY,
                vsam_file TEXT,
                db2_table TEXT,
                field_mappings TEXT,
                created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP
            )
        ''')
        
        conn.commit()
        conn.close()
        logger.info("Database initialized successfully")
    
    def setup_db2_connection(self):
        """Setup DB2 connection (mock if driver not available)"""
        try:
            # In production, use actual DB2 connection string
            # conn_str = "DATABASE=SAMPLE;HOSTNAME=localhost;PORT=50000;PROTOCOL=TCPIP;UID=db2inst1;PWD=password;"
            # self.db2_conn = ibm_db.connect(conn_str, "", "")
            logger.info("DB2 connection setup (mock mode)")
        except Exception as e:
            logger.warning(f"DB2 connection failed: {e}")
    
    def query_sqlite(self, query: str) -> List[Dict[str, Any]]:
        """Execute SQLite query and return results"""
        try:
            conn = sqlite3.connect(self.db_path)
            cursor = conn.cursor()
            cursor.execute(query)
            
            columns = [description[0] for description in cursor.description]
            rows = cursor.fetchall()
            
            results = []
            for row in rows:
                results.append(dict(zip(columns, row)))
            
            conn.close()
            return results
        except Exception as e:
            logger.error(f"SQLite query error: {e}")
            return []
    
    def query_db2_table(self, table_name: str, limit: int = 10) -> Dict[str, Any]:
        """Query DB2 table for sample data"""
        # Mock implementation - replace with actual DB2 queries
        mock_data = {
            'CUSTOMER_MASTER': {
                'columns': ['CUST_ID', 'FIRST_NAME', 'LAST_NAME', 'STATUS', 'CREATED_DATE'],
                'sample_data': [
                    ['C001', 'John', 'Doe', 'ACTIVE', '2024-01-15'],
                    ['C002', 'Jane', 'Smith', 'INACTIVE', '2024-02-20'],
                    ['C003', 'Bob', 'Johnson', 'ACTIVE', '2024-03-10']
                ]
            },
            'POLICY_MASTER': {
                'columns': ['POLICY_ID', 'CUST_ID', 'POLICY_TYPE', 'PREMIUM', 'STATUS'],
                'sample_data': [
                    ['P001', 'C001', 'AUTO', '1200.00', 'ACTIVE'],
                    ['P002', 'C002', 'HOME', '800.00', 'ACTIVE'],
                    ['P003', 'C001', 'LIFE', '2500.00', 'ACTIVE']
                ]
            }
        }
        
        return mock_data.get(table_name.upper(), {
            'columns': ['No data available'],
            'sample_data': []
        })

class MainframeMCPServer:
    """Main MCP Server for Mainframe Codebase Analysis"""
    
    def __init__(self):
        self.server = Server("mainframe-analysis")
        self.parser = MainframeParser()
        self.embedding_manager = EmbeddingManager()
        self.db_manager = DatabaseManager()
        self.setup_tools()
    
    def setup_tools(self):
        """Register MCP tools"""
        
        # Tool 1: Parse Codebase
        @self.server.call_tool()
        async def parse_codebase(files: List[str]) -> List[Dict[str, Any]]:
            """Parse mainframe codebase files and extract metadata"""
            results = []
            
            for file_path in files:
                file_ext = Path(file_path).suffix.lower()
                
                if file_ext in ['.cbl', '.cob', '.cobol']:
                    result = self.parser.parse_cobol_file(file_path)
                    results.append(result)
                elif file_ext in ['.cpy', '.copy']:
                    result = self.parser.parse_copybook_file(file_path)
                    results.append(result)
                elif file_ext in ['.jcl', '.job']:
                    result = self.parser.parse_jcl_file(file_path)
                    results.append(result)
            
            # Store results in SQLite
            self._store_parsed_results(results)
            return results
        
        # Tool 2: Embed Codebase
        @self.server.call_tool()
        async def embed_codebase(files: List[str]) -> Dict[str, Any]:
            """Generate embeddings for codebase files"""
            success = self.embedding_manager.embed_codebase(files)
            return {
                'success': success,
                'message': f"Embedded {len(files)} files" if success else "Embedding failed"
            }
        
        # Tool 3: Search Embeddings
        @self.server.call_tool()
        async def search_embeddings(text: str, top_k: int = 5) -> List[Dict[str, Any]]:
            """Search embeddings for relevant code chunks"""
            return self.embedding_manager.search_embeddings(text, top_k)
        
        # Tool 4: Query SQLite
        @self.server.call_tool()
        async def query_sqlite(query: str) -> List[Dict[str, Any]]:
            """Execute SQLite query on metadata"""
            return self.db_manager.query_sqlite(query)
        
        # Tool 5: Query DB2 Table
        @self.server.call_tool()
        async def query_db2_table(table_name: str, limit: int = 10) -> Dict[str, Any]:
            """Query DB2 table for sample data"""
            return self.db_manager.query_db2_table(table_name, limit)
        
        # Tool 6: Compare VSAM to DB2
        @self.server.call_tool()
        async def compare_vsam_to_db2(vsam_file_path: str, db2_table_name: str, limit: int = 10) -> Dict[str, Any]:
            """Compare VSAM CSV file with DB2 table"""
            return self._compare_vsam_db2(vsam_file_path, db2_table_name, limit)
        
        # Tool 7: Generate Mermaid Graph
        @self.server.call_tool()
        async def get_mermaid_graph(entity: str) -> str:
            """Generate Mermaid diagram for entity relationships"""
            return self._generate_mermaid_graph(entity)
        
        # Tool 8: Generate Friendly Names
        @self.server.call_tool()
        async def generate_friendly_names() -> Dict[str, str]:
            """Generate friendly names for all entities"""
            return self._generate_friendly_names()
    
    def _store_parsed_results(self, results: List[Dict[str, Any]]):
        """Store parsed results in SQLite database"""
        conn = sqlite3.connect(self.db_manager.db_path)
        cursor = conn.cursor()
        
        for result in results:
            if 'program_id' in result:
                # COBOL program
                cursor.execute('''
                    INSERT OR REPLACE INTO programs 
                    (program_id, file_path, friendly_name, copybooks, db2_tables, vsam_files, line_count)
                    VALUES (?, ?, ?, ?, ?, ?, ?)
                ''', (
                    result['program_id'],
                    result['file_path'],
                    self._make_friendly_name(result['program_id']),
                    json.dumps(result.get('copybooks', [])),
                    json.dumps(result.get('db2_tables', [])),
                    json.dumps(result.get('vsam_files', [])),
                    result.get('line_count', 0)
                ))
            elif 'copybook_name' in result:
                # Copybook
                cursor.execute('''
                    INSERT OR REPLACE INTO copybooks 
                    (copybook_name, file_path, friendly_name, fields, line_count)
                    VALUES (?, ?, ?, ?, ?)
                ''', (
                    result['copybook_name'],
                    result['file_path'],
                    self._make_friendly_name(result['copybook_name']),
                    json.dumps(result.get('fields', [])),
                    result.get('line_count', 0)
                ))
        
        conn.commit()
        conn.close()
    
    def _compare_vsam_db2(self, vsam_file_path: str, db2_table_name: str, limit: int = 10) -> Dict[str, Any]:
        """Compare VSAM CSV with DB2 table"""
        try:
            # Read VSAM CSV
            vsam_df = pd.read_csv(vsam_file_path)
            vsam_columns = set(vsam_df.columns)
            
            # Get DB2 table info
            db2_info = self.db_manager.query_db2_table(db2_table_name, limit)
            db2_columns = set(db2_info.get('columns', []))
            
            # Compare columns
            missing_in_db2 = list(vsam_columns - db2_columns)
            extra_in_db2 = list(db2_columns - vsam_columns)
            matching_columns = list(vsam_columns & db2_columns)
            
            # Sample data comparison
            vsam_sample = vsam_df.head(limit).to_dict('records')
            db2_sample = db2_info.get('sample_data', [])
            
            return {
                'vsam_file': vsam_file_path,
                'db2_table': db2_table_name,
                'column_comparison': {
                    'missing_in_db2': missing_in_db2,
                    'extra_in_db2': extra_in_db2,
                    'matching_columns': matching_columns,
                    'match_percentage': len(matching_columns) / len(vsam_columns | db2_columns) * 100
                },
                'sample_data': {
                    'vsam_sample': vsam_sample,
                    'db2_sample': db2_sample
                }
            }
        except Exception as e:
            return {'error': f"Comparison failed: {str(e)}"}
    
    def _generate_mermaid_graph(self, entity: str) -> str:
        """Generate Mermaid diagram for entity relationships"""
        # Simple example - can be enhanced
        return f"""
graph TD
    A[{entity}] --> B[COBOL Programs]
    A --> C[Copybooks]
    A --> D[DB2 Tables]
    A --> E[VSAM Files]
    B --> F[Customer Update Batch]
    C --> G[Customer Common Copy]
    D --> H[CUSTOMER_MASTER]
    E --> I[CUSTOMER_VSAM]
"""
    
    def _generate_friendly_names(self) -> Dict[str, str]:
        """Generate friendly names for entities"""
        friendly_names = {
            'CUSTUPDT': 'Customer Update Batch',
            'POLUPDT': 'Policy Update Batch',
            'CUSTCOPY': 'Customer Common Copy',
            'POLCOPY': 'Policy Common Copy',
            'CUSTOMER_MASTER': 'Customer Master Table',
            'POLICY_MASTER': 'Policy Master Table'
        }
        return friendly_names
    
    def _make_friendly_name(self, technical_name: str) -> str:
        """Convert technical name to friendly name"""
        name_map = self._generate_friendly_names()
        return name_map.get(technical_name.upper(), technical_name.title())
    
    async def run(self):
        """Run the MCP server"""
        from mcp.server.stdio import stdio_server
        
        async with stdio_server() as (read_stream, write_stream):
            await self.server.run(
                read_stream,
                write_stream,
                InitializationOptions(
                    server_name="mainframe-analysis",
                    server_version="1.0.0",
                    capabilities=self.server.get_capabilities(
                        notification_options=NotificationOptions(),
                        experimental_capabilities={}
                    )
                )
            )

def main():
    """Main entry point"""
    server = MainframeMCPServer()
    asyncio.run(server.run())

if __name__ == "__main__":
    main()