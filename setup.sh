#!/bin/bash

# Mainframe Codebase Analysis MVP - Deployment Script
# This script sets up the entire system on a restricted Unix server

set -e  # Exit on any error

# Configuration
PROJECT_NAME="mainframe-analysis-mvp"
PYTHON_VERSION="3.10"
VENV_NAME="venv"
DATA_DIR="data"
MODELS_DIR="models"
LOGS_DIR="logs"

# Colors for output
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
NC='\033[0m' # No Color

# Logging function
log() {
    echo -e "${GREEN}[$(date +'%Y-%m-%d %H:%M:%S')] $1${NC}"
}

warn() {
    echo -e "${YELLOW}[$(date +'%Y-%m-%d %H:%M:%S')] WARNING: $1${NC}"
}

error() {
    echo -e "${RED}[$(date +'%Y-%m-%d %H:%M:%S')] ERROR: $1${NC}"
    exit 1
}

# Check if running as root
check_permissions() {
    if [[ $EUID -eq 0 ]]; then
        warn "Running as root. Consider using a regular user account."
    fi
}

# Check system requirements
check_requirements() {
    log "Checking system requirements..."
    
    # Check Python version
    if ! command -v python3 &> /dev/null; then
        error "Python 3 is not installed. Please install Python 3.8+."
    fi
    
    PYTHON_VER=$(python3 -c "import sys; print('.'.join(map(str, sys.version_info[:2])))")
    log "Found Python version: $PYTHON_VER"
    
    # Check pip
    if ! command -v pip3 &> /dev/null; then
        error "pip3 is not installed. Please install pip."
    fi
    
    # Check available disk space (need at least 2GB for models)
    AVAILABLE_SPACE=$(df . | tail -1 | awk '{print $4}')
    if [ "$AVAILABLE_SPACE" -lt 2097152 ]; then  # 2GB in KB
        warn "Low disk space. Need at least 2GB for embedding models."
    fi
    
    # Check memory (need at least 4GB for embedding models)
    TOTAL_MEM=$(free -m | awk 'NR==2{printf "%.0f", $2/1024}')
    if [ "$TOTAL_MEM" -lt 4 ]; then
        warn "Low memory ($TOTAL_MEM GB). Recommend at least 4GB for optimal performance."
    fi
    
    log "System requirements check completed."
}

# Create project structure
create_project_structure() {
    log "Creating project structure..."
    
    # Create main directories
    mkdir -p {$DATA_DIR,$MODELS_DIR,$LOGS_DIR,codebase,tests,docs}
    mkdir -p codebase/{cobol,jcl,copybooks,xml,db2,vsam}
    mkdir -p embeddings/{faiss,metadata}
    
    # Set permissions
    chmod 755 $DATA_DIR $MODELS_DIR $LOGS_DIR
    chmod 755 codebase codebase/*
    
    log "Project structure created successfully."
}

# Setup Python virtual environment
setup_python_env() {
    log "Setting up Python virtual environment..."
    
    # Create virtual environment
    python3 -m venv $VENV_NAME
    
    # Activate virtual environment
    source $VENV_NAME/bin/activate
    
    # Upgrade pip
    pip install --upgrade pip
    
    log "Python virtual environment created and activated."
}

# Install Python dependencies
install_dependencies() {
    log "Installing Python dependencies..."
    
    # Ensure we're in virtual environment
    if [[ "$VIRTUAL_ENV" == "" ]]; then
        source $VENV_NAME/bin/activate
    fi
    
    # Install core dependencies
    pip install --no-cache-dir \
        "mcp>=1.0.0" \
        "pandas>=1.5.0" \
        "numpy>=1.24.0" \
        "sentence-transformers>=2.2.0" \
        "faiss-cpu>=1.7.0" \
        "torch>=2.0.0" \
        "transformers>=4.30.0" \
        "antlr4-python3-runtime>=4.13.0"
    
    # Install optional dependencies (if available)
    pip install --no-cache-dir \
        "pytest>=7.0.0" \
        "pytest-asyncio>=0.21.0" \
        "black>=23.0.0" \
        "flake8>=6.0.0" || warn "Development dependencies not installed"
    
    # Try to install DB2 drivers (optional)
    pip install ibm-db ibm-db-sa || warn "IBM DB2 drivers not installed (optional)"
    
    log "Python dependencies installed successfully."
}

# Download embedding models for offline use
download_models() {
    log "Downloading embedding models for offline use..."
    
    # Ensure we're in virtual environment
    if [[ "$VIRTUAL_ENV" == "" ]]; then
        source $VENV_NAME/bin/activate
    fi
    
    # Download small CPU-friendly model
    python3 -c "
import os
from sentence_transformers import SentenceTransformer
os.makedirs('$MODELS_DIR', exist_ok=True)
try:
    print('Downloading all-MiniLM-L6-v2 model...')
    model = SentenceTransformer('all-MiniLM-L6-v2')
    model.save('$MODELS_DIR/all-MiniLM-L6-v2')
    print('Model downloaded successfully')
except Exception as e:
    print(f'Error downloading model: {e}')
    exit(1)
"
    
    # Set offline environment variables
    echo "export TRANSFORMERS_OFFLINE=1" >> $VENV_NAME/bin/activate
    echo "export HF_DATASETS_OFFLINE=1" >> $VENV_NAME/bin/activate
    echo "export SENTENCE_TRANSFORMERS_HOME=$(pwd)/$MODELS_DIR" >> $VENV_NAME/bin/activate
    
    log "Embedding models downloaded and configured for offline use."
}

# Initialize database
init_database() {
    log "Initializing SQLite database..."
    
    # Ensure we're in virtual environment
    if [[ "$VIRTUAL_ENV" == "" ]]; then
        source $VENV_NAME/bin/activate
    fi
    
    # Initialize database
    python3 -c "
from mcp_server import DatabaseManager
try:
    db = DatabaseManager('$DATA_DIR/mainframe_analysis.db')
    print('Database initialized successfully')
except Exception as e:
    print(f'Error initializing database: {e}')
    exit(1)
"
    
    # Set database permissions
    chmod 644 $DATA_DIR/mainframe_analysis.db
    
    log "Database initialized successfully."
}

# Create sample data
create_sample_data() {
    log "Creating sample mainframe data..."
    
    # Sample COBOL program
    cat > codebase/cobol/CUSTUPDT.cbl << 'EOF'
       IDENTIFICATION DIVISION.
       PROGRAM-ID. CUSTUPDT.
       
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT CUSTOMER-FILE ASSIGN TO CUSTFILE.
       
       DATA DIVISION.
       FILE SECTION.
       FD CUSTOMER-FILE.
       01 CUSTOMER-REC PIC X(100).
       
       WORKING-STORAGE SECTION.
       COPY CUSTCOPY.
       COPY ADDRCOPY.
       
       01 WS-COUNTERS.
           05 WS-READ-COUNT        PIC 9(8) VALUE ZEROS.
           05 WS-UPDATE-COUNT      PIC 9(8) VALUE ZEROS.
       
       PROCEDURE DIVISION.
       MAIN-PROCESS.
           PERFORM INITIALIZATION
           PERFORM PROCESS-CUSTOMERS
           PERFORM FINALIZATION
           STOP RUN.
           
       INITIALIZATION.
           OPEN INPUT CUSTOMER-FILE
           MOVE ZEROS TO WS-READ-COUNT WS-UPDATE-COUNT.
           
       PROCESS-CUSTOMERS.
           PERFORM UNTIL EOF-CUSTOMER-FILE
               READ CUSTOMER-FILE INTO CUSTOMER-RECORD
               ADD 1 TO WS-READ-COUNT
               
               IF CUSTOMER-STATUS = 'PENDING'
                   MOVE 'ACTIVE' TO CUSTOMER-STATUS
                   EXEC SQL
                       UPDATE CUSTOMER_MASTER 
                       SET STATUS = :CUSTOMER-STATUS,
                           LAST_UPDATE = CURRENT_TIMESTAMP
                       WHERE CUST_ID = :CUSTOMER-ID
                   END-EXEC
                   ADD 1 TO WS-UPDATE-COUNT
               END-IF
               
               EXEC SQL
                   INSERT INTO AUDIT_LOG
                   VALUES (:CUSTOMER-ID, 'STATUS_UPDATE', 
                          :WS-UPDATE-COUNT, CURRENT_TIMESTAMP)
               END-EXEC
           END-PERFORM.
           
       FINALIZATION.
           CLOSE CUSTOMER-FILE
           DISPLAY 'RECORDS READ: ' WS-READ-COUNT
           DISPLAY 'RECORDS UPDATED: ' WS-UPDATE-COUNT.
EOF

    # Sample copybook
    cat > codebase/copybooks/CUSTCOPY.cpy << 'EOF'
      ******************************************************************
      * CUSTOMER RECORD COPYBOOK                                       *
      * USED BY: CUSTUPDT, CUSTRPT, CUSTINQ                          *
      ******************************************************************
       01  CUSTOMER-RECORD.
           05  CUSTOMER-ID          PIC 9(8).
           05  CUSTOMER-NAME.
               10  FIRST-NAME       PIC X(15).
               10  LAST-NAME        PIC X(20).
           05  CUSTOMER-ADDRESS.
               10  STREET-ADDR      PIC X(30).
               10  CITY             PIC X(20).
               10  STATE            PIC X(2).
               10  ZIP-CODE         PIC 9(5).
           05  CUSTOMER-STATUS      PIC X(10).
           05  CUSTOMER-BALANCE     PIC S9(10)V99 COMP-3.
           05  LAST-UPDATE-DATE     PIC 9(8).
           05  CREATED-DATE         PIC 9(8).
           05  EOF-CUSTOMER-FILE    PIC X VALUE 'N'.
               88  EOF-CUSTOMER-FILE VALUE 'Y'.
EOF

    # Sample address copybook
    cat > codebase/copybooks/ADDRCOPY.cpy << 'EOF'
      ******************************************************************
      * ADDRESS VALIDATION COPYBOOK                                    *
      ******************************************************************
       01  ADDRESS-VALIDATION.
           05  ADDR-VALID-FLAG      PIC X VALUE 'N'.
               88  ADDR-VALID       VALUE 'Y'.
               88  ADDR-INVALID     VALUE 'N'.
           05  ADDR-ERROR-CODE      PIC 9(3).
           05  ADDR-ERROR-MSG       PIC X(50).
EOF

    # Sample JCL job
    cat > codebase/jcl/CUSTJOB.jcl << 'EOF'
//CUSTJOB  JOB (ACCT),'CUSTOMER UPDATE',CLASS=A,MSGCLASS=H,
//             NOTIFY=&SYSUID,TIME=(0,30)
//*
//* CUSTOMER MASTER FILE UPDATE JOB
//* RUNS DAILY AT 2:00 AM
//*
//STEP010  EXEC PGM=CUSTUPDT,PARM='UPDATE,PROD'
//STEPLIB  DD DSN=PROD.LOAD.LIBRARY,DISP=SHR
//CUSTFILE DD DSN=PROD.CUSTOMER.MASTER,DISP=SHR
//AUDITLOG DD DSN=PROD.AUDIT.LOG,DISP=MOD,
//             UNIT=SYSDA,SPACE=(CYL,(10,5),RLSE)
//SYSOUT   DD SYSOUT=*
//SYSPRINT DD SYSOUT=*
//SYSUDUMP DD SYSOUT=Y
//SYSIN    DD *
UPDATE MODE=BATCH
VALIDATE=YES
BACKUP=YES
/*
//*
//STEP020  EXEC PGM=CUSTRPT,PARM='SUMMARY'
//STEPLIB  DD DSN=PROD.LOAD.LIBRARY,DISP=SHR
//CUSTFILE DD DSN=PROD.CUSTOMER.MASTER,DISP=SHR
//REPORT   DD DSN=PROD.REPORTS.DAILY(+1),DISP=(NEW,CATLG),
//             UNIT=SYSDA,SPACE=(TRK,(50,10),RLSE),
//             DCB=(RECFM=FB,LRECL=133,BLKSIZE=1330)
//SYSOUT   DD SYSOUT=*
//*
EOF

    # Sample VSAM data
    cat > $DATA_DIR/CUSTOMER_VSAM.csv << 'EOF'
CUST_ID,FIRST_NAME,LAST_NAME,STREET_ADDR,CITY,STATE,ZIP_CODE,STATUS,BALANCE,LAST_UPDATE,CREATED_DATE
00012345,John,Doe,123 Main St,Springfield,IL,62701,ACTIVE,1250.50,20240315,20240101
00012346,Jane,Smith,456 Oak Ave,Chicago,IL,60601,ACTIVE,750.25,20240320,20240105
00012347,Bob,Johnson,789 Pine Rd,Peoria,IL,61601,INACTIVE,0.00,20240310,20240110
00012348,Alice,Williams,321 Elm St,Rockford,IL,61101,PENDING,500.75,20240325,20240115
00012349,Charlie,Brown,654 Maple Dr,Aurora,IL,60502,ACTIVE,2100.00,20240330,20240120
EOF

    # Sample policy data
    cat > $DATA_DIR/POLICY_VSAM.csv << 'EOF'
POLICY_ID,CUST_ID,POLICY_TYPE,PREMIUM,STATUS,EFFECTIVE_DATE,EXPIRY_DATE
POL001,00012345,AUTO,1200.00,ACTIVE,20240101,20241231
POL002,00012346,HOME,800.00,ACTIVE,20240201,20250131
POL003,00012345,LIFE,2500.00,ACTIVE,20240101,20341231
POL004,00012347,AUTO,950.00,CANCELLED,20240110,20240310
POL005,00012349,HOME,1100.00,ACTIVE,20240120,20250119
EOF

    log "Sample mainframe data created successfully."
}

# Setup GitHub Copilot configuration
setup_copilot_config() {
    log "Setting up GitHub Copilot MCP configuration..."
    
    # Create VS Code directory if it doesn't exist
    mkdir -p .vscode
    
    # Create MCP configuration
    cat > .vscode/mcp_config.json << EOF
{
  "mcpServers": {
    "mainframe-analysis": {
      "command": "$(pwd)/$VENV_NAME/bin/python",
      "args": ["$(pwd)/mcp_server.py"],
      "cwd": "$(pwd)",
      "env": {
        "PYTHONPATH": "$(pwd)",
        "LOG_LEVEL": "INFO",
        "DATABASE_PATH": "$(pwd)/$DATA_DIR/mainframe_analysis.db",
        "MODELS_PATH": "$(pwd)/$MODELS_DIR",
        "TRANSFORMERS_OFFLINE": "1",
        "HF_DATASETS_OFFLINE": "1"
      }
    }
  }
}
EOF

    # Create VS Code settings
    cat > .vscode/settings.json << EOF
{
  "github.copilot.advanced": {
    "mcp": {
      "configFile": "$(pwd)/.vscode/mcp_config.json"
    }
  },
  "python.defaultInterpreterPath": "$(pwd)/$VENV_NAME/bin/python",
  "python.terminal.activateEnvironment": true
}
EOF

    log "GitHub Copilot MCP configuration created."
}

# Run initial tests
run_tests() {
    log "Running initial system tests..."
    
    # Ensure we're in virtual environment
    if [[ "$VIRTUAL_ENV" == "" ]]; then
        source $VENV_NAME/bin/activate
    fi
    
    # Test MCP server components
    python3 -c "
import sys
sys.path.append('.')

# Test imports
try:
    from mcp_server import MainframeParser, EmbeddingManager, DatabaseManager
    print('✓ All imports successful')
except Exception as e:
    print(f'✗ Import error: {e}')
    exit(1)

# Test parser
try:
    parser = MainframeParser()
    result = parser.parse_cobol_file('codebase/cobol/CUSTUPDT.cbl')
    print(f'✓ Parser test: Found program {result[\"program_id\"]}')
except Exception as e:
    print(f'✗ Parser error: {e}')
    exit(1)

# Test database
try:
    db = DatabaseManager('$DATA_DIR/mainframe_analysis.db')
    tables = db.query_sqlite('SELECT name FROM sqlite_master WHERE type=\"table\"')
    print(f'✓ Database test: Found {len(tables)} tables')
except Exception as e:
    print(f'✗ Database error: {e}')
    exit(1)

# Test embedding manager (basic)
try:
    em = EmbeddingManager()
    chunks = em.chunk_text('This is a test\\nWith multiple lines\\nFor chunking')
    print(f'✓ Embedding test: Created {len(chunks)} chunks')
except Exception as e:
    print(f'✗ Embedding error: {e}')
    exit(1)

print('All basic tests passed!')
"

    if [ $? -eq 0 ]; then
        log "Initial tests passed successfully."
    else
        error "Initial tests failed. Check the output above."
    fi
}

# Create systemd service (optional)
create_systemd_service() {
    log "Creating systemd service (optional)..."
    
    # Check if systemd is available
    if ! command -v systemctl &> /dev/null; then
        warn "systemctl not found. Skipping systemd service creation."
        return
    fi
    
    # Create service file
    cat > mainframe-mcp.service << EOF
[Unit]
Description=Mainframe Codebase Analysis MCP Server
After=network.target

[Service]
Type=simple
User=$(whoami)
WorkingDirectory=$(pwd)
ExecStart=$(pwd)/$VENV_NAME/bin/python $(pwd)/mcp_server.py
Restart=always
RestartSec=10
Environment=PYTHONPATH=$(pwd)
Environment=LOG_LEVEL=INFO
Environment=DATABASE_PATH=$(pwd)/$DATA_DIR/mainframe_analysis.db
Environment=MODELS_PATH=$(pwd)/$MODELS_DIR
Environment=TRANSFORMERS_OFFLINE=1
Environment=HF_DATASETS_OFFLINE=1

[Install]
WantedBy=multi-user.target
EOF

    echo "Systemd service file created: mainframe-mcp.service"
    echo "To install and start the service:"
    echo "  sudo cp mainframe-mcp.service /etc/systemd/system/"
    echo "  sudo systemctl enable mainframe-mcp"
    echo "  sudo systemctl start mainframe-mcp"
}

# Create management scripts
create_management_scripts() {
    log "Creating management scripts..."
    
    # Start script
    cat > start_server.sh << 'EOF'
#!/bin/bash
cd "$(dirname "$0")"
source venv/bin/activate
export PYTHONPATH="$(pwd)"
export DATABASE_PATH="$(pwd)/data/mainframe_analysis.db"
export MODELS_PATH="$(pwd)/models"
export TRANSFORMERS_OFFLINE=1
export HF_DATASETS_OFFLINE=1

echo "Starting Mainframe Analysis MCP Server..."
python mcp_server.py
EOF

    # Stop script
    cat > stop_server.sh << 'EOF'
#!/bin/bash
echo "Stopping Mainframe Analysis MCP Server..."
pkill -f "python.*mcp_server.py"
echo "Server stopped."
EOF

    # Status script
    cat > status_server.sh << 'EOF'
#!/bin/bash
if pgrep -f "python.*mcp_server.py" > /dev/null; then
    echo "✓ MCP Server is running"
    echo "PID: $(pgrep -f 'python.*mcp_server.py')"
else
    echo "✗ MCP Server is not running"
fi
EOF

    # Test script
    cat > test_system.sh << 'EOF'
#!/bin/bash
cd "$(dirname "$0")"
source venv/bin/activate
export PYTHONPATH="$(pwd)"

echo "Running system tests..."
python test_examples.py --test

echo -e "\nRunning usage examples..."
python test_examples.py --examples
EOF

    # Parse codebase script
    cat > parse_codebase.sh << 'EOF'
#!/bin/bash
cd "$(dirname "$0")"
source venv/bin/activate
export PYTHONPATH="$(pwd)"

if [ $# -eq 0 ]; then
    echo "Usage: $0 <codebase_directory>"
    echo "Example: $0 /path/to/mainframe/code"
    exit 1
fi

CODEBASE_DIR="$1"

if [ ! -d "$CODEBASE_DIR" ]; then
    echo "Error: Directory $CODEBASE_DIR does not exist"
    exit 1
fi

echo "Parsing codebase from: $CODEBASE_DIR"

python -c "
import os
import sys
import asyncio
from pathlib import Path
from mcp_server import MainframeMCPServer

async def parse_directory(directory):
    server = MainframeMCPServer()
    
    # Find all relevant files
    extensions = ['.cbl', '.cob', '.cobol', '.cpy', '.copy', '.jcl', '.job']
    files = []
    
    for ext in extensions:
        files.extend(Path(directory).rglob(f'*{ext}'))
    
    file_paths = [str(f) for f in files]
    print(f'Found {len(file_paths)} files to parse')
    
    if file_paths:
        # Parse files
        results = await server.parse_codebase(file_paths)
        print(f'Parsed {len(results)} files successfully')
        
        # Generate embeddings
        embed_result = await server.embed_codebase(file_paths)
        print(f'Embedding result: {embed_result}')
        
        # Generate friendly names
        friendly_names = await server.generate_friendly_names()
        print(f'Generated {len(friendly_names)} friendly names')
        
        print('Codebase parsing completed!')
    else:
        print('No relevant files found in the specified directory')

asyncio.run(parse_directory('$CODEBASE_DIR'))
"
EOF

    # Make scripts executable
    chmod +x start_server.sh stop_server.sh status_server.sh test_system.sh parse_codebase.sh
    
    log "Management scripts created successfully."
}

# Create documentation
create_documentation() {
    log "Creating documentation..."
    
    cat > README.md << 'EOF'
# Mainframe Codebase Analysis MVP

This system provides natural language querying capabilities for mainframe codebases using GitHub Copilot and MCP (Model Context Protocol).

## Features

- **Code Parsing**: COBOL, JCL, Copybooks, XML, DB2 DDL parsing using ANTLR4
- **Semantic Search**: Offline embedding-based search with FAISS
- **Structured Queries**: SQLite-based metadata queries
- **VSAM ↔ DB2 Comparison**: Data structure and content comparison
- **Impact Analysis**: Relationship diagrams with Mermaid
- **Natural Language Interface**: GitHub Copilot integration

## Quick Start

1. **Start the MCP server**:
   ```bash
   ./start_server.sh
   ```

2. **Parse your codebase**:
   ```bash
   ./parse_codebase.sh /path/to/your/mainframe/code
   ```

3. **Open VS Code and use Copilot** with queries like:
   - "Which programs update CUSTOMER-STATUS?"
   - "Compare CUSTOMER_VSAM.csv with DB2 CUSTOMER_MASTER table"
   - "Show me sample data from tables updated by Customer Update Batch"

## Management Commands

- `./start_server.sh` - Start the MCP server
- `./stop_server.sh` - Stop the MCP server  
- `./status_server.sh` - Check server status
- `./test_system.sh` - Run tests and examples
- `./parse_codebase.sh <dir>` - Parse a codebase directory

## Directory Structure

```
mainframe-analysis-mvp/
├── mcp_server.py           # Main MCP server
├── test_examples.py        # Tests and examples
├── data/                   # SQLite DB and VSAM files
├── models/                 # Offline embedding models
├── codebase/               # Sample mainframe code
│   ├── cobol/             # COBOL programs
│   ├── copybooks/         # Copybook files
│   ├── jcl/               # JCL jobs
│   └── vsam/              # VSAM data files
├── embeddings/            # FAISS indices
└── logs/                  # Application logs
```

## Configuration

The system is configured for offline operation in restricted environments:
- Embedding models are pre-downloaded
- No external network access required
- All data stored locally in SQLite

## Troubleshooting

1. **Server won't start**: Check logs in `logs/` directory
2. **Import errors**: Ensure virtual environment is activated
3. **Model loading fails**: Re-run deployment script to download models
4. **Copilot not working**: Check `.vscode/mcp_config.json` paths

For more details, see the implementation guide in the deployment artifacts.
EOF

    cat > USAGE_EXAMPLES.md << 'EOF'
# Usage Examples

## Natural Language Queries

### 1. Finding Programs
**Query**: "Which programs update CUSTOMER-STATUS?"

**What Copilot does**:
1. Searches semantic embeddings for "customer status update"
2. Queries SQLite for programs accessing customer tables
3. Returns programs with friendly names and code snippets

### 2. Data Comparison
**Query**: "Compare CUSTOMER_VSAM.csv with DB2 CUSTOMER_MASTER table"

**What Copilot does**:
1. Calls compare_vsam_to_db2 tool
2. Analyzes column differences
3. Shows sample data comparison
4. Reports compatibility percentage

### 3. Sample Data Analysis
**Query**: "Show me sample rows from tables updated by Customer Update Batch"

**What Copilot does**:
1. Identifies the program (Customer Update Batch = CUSTUPDT)
2. Finds related DB2 tables
3. Queries sample data from those tables
4. Presents formatted results

### 4. Impact Analysis
**Query**: "Generate impact diagram for POLICY-COMMON copybook"

**What Copilot does**:
1. Finds programs using the copybook
2. Identifies related tables and files
3. Generates Mermaid relationship diagram
4. Shows visual dependencies

## MCP Tool Examples

### Direct Tool Usage (for advanced users)

```python
# Parse new codebase files
await parse_codebase([
    "path/to/NEWPROG.cbl",
    "path/to/NEWCOPY.cpy"
])

# Search for relevant code
results = await search_embeddings(
    "error handling routines", 
    top_k=10
)

# Query structured metadata  
programs = await query_sqlite("""
    SELECT p.program_id, p.friendly_name 
    FROM programs p 
    WHERE p.db2_tables LIKE '%CUSTOMER%'
""")

# Get DB2 sample data
sample_data = await query_db2_table(
    "CUSTOMER_MASTER", 
    limit=20
)

# Compare VSAM with DB2
comparison = await compare_vsam_to_db2(
    "data/CUSTOMER_VSAM.csv",
    "CUSTOMER_MASTER",
    limit=50
)
```

## Advanced Queries

### Complex Analysis
**Query**: "Analyze the data flow from CUSTUPDT program to CUSTOMER_MASTER table and show any VSAM file dependencies"

**Expected Response**:
1. Program analysis of CUSTUPDT
2. DB2 table interactions
3. VSAM file relationships
4. Data flow diagram
5. Dependency summary

### Migration Planning
**Query**: "What would be the impact of changing CUSTOMER-STATUS field from X(10) to X(15) in CUSTCOPY copybook?"

**Expected Response**:
1. Programs using CUSTCOPY
2. DB2 tables with CUSTOMER-STATUS
3. VSAM files affected
4. Impact assessment
5. Migration recommendations
EOF

    log "Documentation created successfully."
}

# Print final summary
print_summary() {
    echo ""
    echo "========================================"
    echo "🎉 DEPLOYMENT COMPLETED SUCCESSFULLY! 🎉"
    echo "========================================"
    echo ""
    echo "📁 Project Location: $(pwd)"
    echo "🐍 Python Environment: $VENV_NAME"
    echo "💾 Database: $DATA_DIR/mainframe_analysis.db"
    echo "🤖 Models: $MODELS_DIR/"
    echo ""
    echo "🚀 Next Steps:"
    echo "1. Start the MCP server:"
    echo "   ./start_server.sh"
    echo ""
    echo "2. Test the system:"
    echo "   ./test_system.sh"
    echo ""
    echo "3. Parse your codebase:"
    echo "   ./parse_codebase.sh /path/to/your/mainframe/code"
    echo ""
    echo "4. Open VS Code and use Copilot with queries like:"
    echo "   - 'Which programs update CUSTOMER-STATUS?'"
    echo "   - 'Compare CUSTOMER_VSAM.csv with DB2 CUSTOMER_MASTER table'"
    echo ""
    echo "📖 Documentation:"
    echo "   - README.md - Getting started guide"
    echo "   - USAGE_EXAMPLES.md - Example queries"
    echo ""
    echo "🔧 Management Commands:"
    echo "   - ./start_server.sh    # Start MCP server"
    echo "   - ./stop_server.sh     # Stop MCP server"
    echo "   - ./status_server.sh   # Check status"
    echo ""
    echo "✅ System is ready for use!"
    echo ""
}

# Main deployment function
main() {
    echo "🚀 Starting Mainframe Codebase Analysis MVP Deployment"
    echo "========================================================"
    
    check_permissions
    check_requirements
    create_project_structure
    setup_python_env
    install_dependencies
    generate_antlr_parsers
    download_models
    init_database
    create_sample_data
    setup_copilot_config
    run_tests
    create_systemd_service
    create_management_scripts
    create_documentation
    
    print_summary
}

# Handle command line arguments
case "${1:-}" in
    --help|-h)
        echo "Usage: $0 [options]"
        echo ""
        echo "Options:"
        echo "  --help, -h          Show this help message"
        echo "  --quick             Quick setup (skip tests and examples)"
        echo "  --no-models         Skip downloading embedding models"
        echo "  --no-samples        Skip creating sample data"
        echo ""
        echo "This script sets up the complete Mainframe Analysis MVP system."
        exit 0
        ;;
    --quick)
        QUICK_SETUP=true
        ;;
    --no-models)
        SKIP_MODELS=true
        ;;
    --no-samples)
        SKIP_SAMPLES=true
        ;;
esac

# Run main deployment
main

# Final check
if [ $? -eq 0 ]; then
    log "🎯 Deployment completed successfully!"
    exit 0
else
    error "❌ Deployment failed!"
    exit 1
fi