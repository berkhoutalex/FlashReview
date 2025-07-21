#!/bin/bash
# Script to initialize test database for GitHub Actions

# Set PostgreSQL connection details
export PGHOST=${PGHOST:-localhost}
export PGUSER=${PGUSER:-postgres}
export PGPASSWORD=${PGPASSWORD:-postgres}
export PGDATABASE=${PGDATABASE:-flashcards_test}

echo "Setting up test database schema..."

# Import schema if needed (this is an alternative approach if setupSchema in the code doesn't work)
# psql -d $PGDATABASE -f schema.sql

echo "Test database setup complete."
