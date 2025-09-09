# Mainframe Codebase Analysis MVP - Implementation Guide

## Overview
This guide walks you through building a Mainframe Codebase Analysis system that integrates GitHub Copilot with MCP (Model Context Protocol) for analyzing COBOL, JCL, copybooks, XML, CICS/MQ, and DB2 artifacts using natural language queries.

## Prerequisites

### System Requirements
- **Unix/Linux server** (restricted environment, no external network)
- **Python 3.8+**
- **4GB+ RAM** (for embedding models)
- **GitHub Copilot subscription**
- **VS Code** with GitHub Copilot extension

### Required Dependencies
- MCP framework
- ANTLR4 for parsing
- Sentence Transformers (offline embedding models)
- FAISS for vector search
- SQLite for metadata storage
- Pandas for data processing
- Optional: IBM DB2 drivers

## Step 1: Environment Setup

### 1.1 Create Project Structure
```bash
mkdir mainframe-analysis-mvp
cd mainframe-analysis-mvp

# Create directory structure
mkdir -p {data,codebase,models,embeddings,tests,docs}
mkdir -p codebase/{cobol,jcl,copybooks,xml,db2}
```

### 1.2 Set Up Python Virtual Environment
```bash
python3 -m venv venv
source venv/bin/activate  # On Windows: venv\Scripts\activate

# Upgrade pip
pip install --upgrade pip
```

### 1.3 Install Dependencies
```bash
# Install from requirements.txt
pip install -r requirements.txt

# Or install manually:
pip install mcp pandas numpy sentence-transformers faiss-cpu torch transformers antlr4-python3-runtime

# Optional DB2 drivers (if available)
# pip install ibm-db ibm-db-sa
```

### 1.4 Download Offline Embedding Models
```bash
# Download models for offline use
python -c "
from sentence_transformers import SentenceTransformer
# Download small, CPU-friendly model
model = SentenceTransformer('all-MiniLM-L6-v2')
model.save('./models/all-MiniLM-L6-v2')

# Alternative: larger, more accurate model
# model = SentenceTransformer('all-mpnet-base-v2') 
# model.save('./models/all-mpnet-base-v2')
"
```

## Step 2: Core Components Implementation

### 2.1 MCP Server Setup
1. **Copy the MCP server code** from the `mcp_server.py` artifact
2. **Place it in your project root**
3. **Make it executable**:
```bash
chmod +x mcp_server.py
```

### 2.2 Configure GitHub Copilot MCP Integration
1. **Copy the MCP configuration** from the `copilot_config.json` artifact
2. **Place it in VS Code settings directory**:
```bash
# For VS Code user settings
mkdir -p ~/.vscode
cp copilot_config.json ~/.vscode/mcp_config.json

# For workspace settings
mkdir -p .vscode
cp copilot_config.json .vscode/mcp_config.json
```

### 2.3 Initialize Database
```bash
python -c "
from mcp_server import DatabaseManager
db = DatabaseManager()
print('Database initialized successfully')
"
```

## Step 3: ANTLR4 Parser Setup (Enhanced Implementation)

### 3.1 Install ANTLR4 Tools
```bash
# Download ANTLR4 JAR (for grammar compilation)
wget https://www.antlr.org/download/antlr-4.13.1-complete.jar
export CLASSPATH=".:antlr-4.13.1-complete.jar:$CLASSPATH"
alias antlr4='java -jar antlr-4.13.1-complete.jar'
```

### 3.2 Create Grammar Files
Create `grammars/Cobol.g4`:
```antlr
grammar Cobol;

program: identificationDivision environmentDivision? dataDivision? procedureDivision? EOF;

identificationDivision: 'IDENTIFICATION' 'DIVISION' '.' programId;
programId: 'PROGRAM-ID' '.' IDENTIFIER;

dataDivision: 'DATA' 'DIVISION' '.' (workingStorageSection | fileSection)*;
workingStorageSection: 'WORKING-STORAGE' 'SECTION' '.' dataItem*;
fileSection: 'FILE' 'SECTION' '.' fileDescription*;

procedureDivision: 'PROCEDURE' 'DIVISION' '.' statement*;

statement: moveStatement | ifStatement | performStatement | copyStatement;
moveStatement: 'MOVE' expression 'TO' IDENTIFIER;
ifStatement: 'IF' condition statement ('ELSE' statement)? 'END-IF';
performStatement: 'PERFORM' IDENTIFIER;
copyStatement: 'COPY' IDENTIFIER;

// Lexer rules
IDENTIFIER: [A-Z][A-Z0-9-]*;
NUMBER: [0-9]+;
STRING: '"' ~["]* '"';
WS: [ \t\r\n]+ -> skip;
```

### 3.3 Generate Parser Code
```bash
cd grammars
antlr4 -Dlanguage=Python3 Cobol.g4
mv *.py ../parsers/
```

## Step 4: Sample Data Setup

### 4.1 Create Sample Mainframe Files
```bash
# Sample COBOL program
cat > codebase/cobol/CUSTUPDT.cbl << 'EOF'
       IDENTIFICATION DIVISION.
       PROGRAM-ID. CUSTUPDT.
       
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       COPY CUSTCOPY.
       
       PROCEDURE DIVISION.
       MAIN-PROCESS.
           MOVE SPACES TO CUSTOMER-STATUS
           UPDATE CUSTOMER_MASTER 
               SET STATUS = 'ACTIVE'
               WHERE CUST_ID = :CUSTOMER-ID
           .
EOF

# Sample Copybook
cat > codebase/copybooks/CUSTCOPY.cpy << 'EOF'
       01  CUSTOMER-RECORD.
           05  CUSTOMER-ID          PIC 9(8).
           05  CUSTOMER-NAME        PIC X(30).
           05  CUSTOMER-STATUS      PIC X(10).
           05  CUSTOMER-BALANCE     PIC 9(10)V99.
EOF

# Sample JCL
cat > codebase/jcl/CUSTJOB.jcl << 'EOF'
//CUSTJOB  JOB CLASS=A,MSGCLASS=H
//STEP1    EXEC PGM=CUSTUPDT
//CUSTFILE DD DSN=PROD.CUSTOMER.MASTER,DISP=SHR
//SYSOUT   DD SYSOUT=*
EOF

# Sample VSAM data
cat > data/CUSTOMER_VSAM.csv << 'EOF'
CUST_ID,FIRST_NAME,LAST_NAME,STATUS,CREATED_DATE
C001,John,Doe,ACTIVE,2024-01-15
C002,Jane,Smith,INACTIVE,2024-02-20
C003,Bob,Johnson,ACTIVE,2024-03-10
EOF
```

## Step 5: Testing the System

### 5.1 Start MCP Server
```bash
# Terminal 1: Start MCP server
python mcp_server.py
```

### 5.2 Test Individual Components
```bash
# Terminal 2: Test parsing
python -c "
from mcp_server import MainframeParser
parser = MainframeParser()
result = parser.parse_cobol_file('codebase/cobol/CUSTUPDT.cbl')
print('Parsed COBOL:', result)
"

# Test embeddings
python -c "
from mcp_server import EmbeddingManager
em = EmbeddingManager()
em.embed_codebase(['codebase/cobol/CUSTUPDT.cbl'])
results = em.search_embeddings('customer status update')
print('Search results:', results)
"

# Test database
python -c "
from mcp_server import DatabaseManager
db = DatabaseManager()
results = db.query_sqlite('SELECT * FROM programs LIMIT 5')
print('Database results:', results)
"
```

## Step 6: GitHub Copilot Integration

### 6.1 Configure VS Code
1. **Install GitHub Copilot extension**
2. **Add MCP configuration to VS Code settings**:
```json
{
  "github.copilot.advanced": {
    "mcp": {
      "servers": {
        "mainframe-analysis": {
          "command": "python",
          "args": ["mcp_server.py"],
          "cwd": "/path/to/mainframe-analysis-mvp"
        }
      }
    }
  }
}
```

### 6.2 Test Copilot Integration
1. **Open VS Code in project directory**
2. **Open a new file and type natural language queries**:
   - "Which programs update CUSTOMER-STATUS?"
   - "Compare CUSTOMER_VSAM.csv with DB2 CUSTOMER_MASTER table"
   - "Show me sample data from tables updated by Customer Update Batch"

## Step 7: Production Deployment

### 7.1 Docker Deployment
```bash
# Build Docker image
docker build -t mainframe-analysis-mcp .

# Run with docker-compose
docker-compose up -d

# Check logs
docker-compose logs -f mainframe-mcp
```

### 7.2 Configure for Restricted Environment
```bash
# Copy offline models to server
scp -r models/ user@restricted-server:/app/models/

# Set up without internet access
export TRANSFORMERS_OFFLINE=1
export HF_DATASETS_OFFLINE=1
```

## Step 8: Advanced Features Implementation

### 8.1 Enhanced VSAM ↔ DB2 Comparison
```python
# Add to mcp_server.py
def advanced_vsam_db2_comparison(self, vsam_file_path: str, db2_table_name: str):
    # Read VSAM with field type detection
    vsam_df = pd.read_csv(vsam_file_path, dtype=str)
    
    # Analyze data patterns for type inference
    for col in vsam_df.columns:
        sample_values = vsam_df[col].dropna().head(100)
        if sample_values.str.match(r'^\d{4}-\d{2}-\d{2}$').all():
            print(f"{col}: DATE field detected")
        elif sample_values.str.match(r'^\d+$').all():
            print(f"{col}: NUMERIC field detected")
    
    return analysis_results
```

### 8.2 Mermaid Diagram Generation
```python
def generate_detailed_mermaid(self, entity: str) -> str:
    # Query relationships from database
    relationships = self.db_manager.query_sqlite(f"""
        SELECT p.program_id, p.copybooks, p.db2_tables, p.vsam_files
        FROM programs p 
        WHERE p.program_id LIKE '%{entity}%' 
           OR p.db2_tables LIKE '%{entity}%'
    """)
    
    mermaid = "graph TD\n"
    for rel in relationships:
        program = rel['program_id']
        for copybook in json.loads(rel['copybooks'] or '[]'):
            mermaid += f"    {program} --> {copybook}\n"
        for table in json.loads(rel['db2_tables'] or '[]'):
            mermaid += f"    {program} --> {table}\n"
    
    return mermaid
```

## Step 9: Monitoring and Maintenance

### 9.1 Setup Logging
```python
# Enhanced logging configuration
import logging
from logging.handlers import RotatingFileHandler

# Configure rotating log files
handler = RotatingFileHandler(
    'logs/mainframe_analysis.log', 
    maxBytes=10*1024*1024,  # 10MB
    backupCount=5
)
logging.basicConfig(
    level=logging.INFO,
    format='%(asctime)s - %(name)s - %(levelname)s - %(message)s',
    handlers=[handler, logging.StreamHandler()]
)
```

### 9.2 Performance Monitoring
```bash
# Monitor system resources
htop

# Monitor embedding search performance
python -c "
import time
from mcp_server import EmbeddingManager

em = EmbeddingManager()
start = time.time()
results = em.search_embeddings('customer update batch', 10)
print(f'Search took: {time.time() - start:.2f} seconds')
"
```

## Step 10: Usage Examples

### 10.1 Natural Language Queries
Once everything is set up, you can use these queries in GitHub Copilot:

**Query 1**: "Which programs update CUSTOMER-STATUS?"
- Copilot will automatically call `search_embeddings` and `query_sqlite`
- Returns programs with friendly names and code snippets

**Query 2**: "Compare CUSTOMER_VSAM.csv with DB2 CUSTOMER_MASTER table"
- Calls `compare_vsam_to_db2` tool
- Shows column differences and data samples

**Query 3**: "Generate impact diagram for POLICY-COMMON copybook"
- Calls `get_mermaid_graph` tool
- Returns visual relationship diagram

**Query 4**: "Show me sample rows from tables updated by Customer Update Batch"
- Combines multiple tool calls for comprehensive analysis
- Returns friendly names and actual data

## Troubleshooting

### Common Issues and Solutions

1. **MCP Connection Issues**
   ```bash
   # Check MCP server status
   ps aux | grep mcp_server
   
   # Restart server
   pkill -f mcp_server.py
   python mcp_server.py &
   ```

2. **Embedding Model Loading Errors**
   ```bash
   # Check model files
   ls -la models/
   
   # Re-download if needed
   python -c "from sentence_transformers import SentenceTransformer; SentenceTransformer('all-MiniLM-L6-v2')"
   ```

3. **Database Lock Issues**
   ```bash
   # Check for database locks
   lsof mainframe_analysis.db
   
   # Remove if necessary
   rm mainframe_analysis.db
   python -c "from mcp_server import DatabaseManager; DatabaseManager()"
   ```

4. **ANTLR4 Parser Errors**
   ```bash
   # Regenerate parsers
   cd grammars
   antlr4 -Dlanguage=Python3 Cobol.g4
   ```

## Next Steps

1. **Enhance Parsers**: Add more sophisticated COBOL, JCL, and XML parsing
2. **Improve Embeddings**: Experiment with domain-specific embedding models
3. **Add More Tools**: Implement call graph analysis, dead code detection
4. **Web Interface**: Build a web UI for non-Copilot users
5. **Performance Optimization**: Add caching and parallel processing
6. **Security**: Implement access controls and audit logging

## Support and Resources

- **MCP Documentation**: [MCP Protocol Specification]
- **ANTLR4 Documentation**: [ANTLR4 Grammar Guide]
- **Sentence Transformers**: [Offline Model Guide]
- **GitHub Copilot**: [Extension Documentation]

This implementation provides a solid foundation for mainframe codebase analysis using modern AI tools while respecting the constraints of restricted environments.