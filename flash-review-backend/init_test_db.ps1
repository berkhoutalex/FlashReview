# init_test_db.ps1 - PowerShell script to initialize test database for flashcards app

# Set PostgreSQL connection details
$env:PGHOST = if ($env:PGHOST) { $env:PGHOST } else { "localhost" }
$env:PGUSER = if ($env:PGUSER) { $env:PGUSER } else { "postgres" }
$env:PGPASSWORD = if ($env:PGPASSWORD) { $env:PGPASSWORD } else { "postgres" }

# Create test database
Write-Host "Creating test database..."
& psql -c "DROP DATABASE IF EXISTS flashcards_test;"
& psql -c "CREATE DATABASE flashcards_test;"

Write-Host "Test database created successfully."
