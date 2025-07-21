#!/bin/bash
# init_test_db.sh - Script to initialize test database for flashcards app

# Set PostgreSQL connection details
export PGHOST=${PGHOST:-localhost}
export PGUSER=${PGUSER:-postgres}
export PGPASSWORD=${PGPASSWORD:-postgres}

# Create test database
echo "Creating test database..."
psql -c "DROP DATABASE IF EXISTS flashcards_test;"
psql -c "CREATE DATABASE flashcards_test;"

echo "Test database created successfully."
