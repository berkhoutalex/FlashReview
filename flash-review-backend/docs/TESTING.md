# Testing Documentation for FlashReview Backend

This document explains the testing setup for the FlashReview backend application and how the automated tests are run in the CI/CD pipeline.

## Test Structure

The test suite is organized into several key areas:

1. **Database Tests**: Tests for database operations including:
   - User creation and authentication
   - Flashcard CRUD operations
   - Review processing
   - Statistics retrieval

2. **API Tests**: Tests for data serialization/deserialization, ensuring that:
   - JSON encoding and decoding works correctly for all data types
   - API request and response formats are valid

3. **Server Tests**: Tests for the server endpoints, covering:
   - Authentication and authorization
   - Flashcard operations
   - Review operations
   - Statistical queries

## Running Tests Locally

To run the tests locally:

1. **Set up the test database**:

   On Linux/Mac:
   ```bash
   ./init_test_db.sh
   ```

   On Windows:
   ```powershell
   .\init_test_db.ps1
   ```

2. **Run the test suite**:
   ```bash
   stack test
   ```

## CI/CD Integration

The project is configured with GitHub Actions to automatically run tests on:
- Every push to the `main` branch
- Every pull request targeting the `main` branch
- Manual triggers via the GitHub UI

### GitHub Actions Workflow

The workflow performs these steps:

1. **Set up environment**:
   - Checks out the code
   - Sets up Haskell with Stack
   - Configures caching for dependencies

2. **Database preparation**:
   - Spins up a PostgreSQL container
   - Creates a test database
   - Initializes the schema

3. **Build and test**:
   - Builds dependencies
   - Builds the project
   - Runs the test suite

### Workflow Configuration

The workflow configuration is located in `.github/workflows/backend-tests.yml`. Key features include:

- **PostgreSQL service container** with health checks to ensure the database is ready
- **Caching** of Stack dependencies to speed up builds
- **Environment variables** for database connection
- **Working directory** configuration to run commands in the backend directory

## Adding New Tests

When adding new functionality, please add corresponding tests:

1. For new database functions, add tests in `test/DatabaseSpec.hs`
2. For new API types, add serialization tests in `test/APISpec.hs`
3. For new endpoints, add server tests in `test/ServerSpec.hs`

## Test Database Configuration

The test database uses these default settings:
- Host: localhost
- Database: flashcards_test
- User: postgres
- Password: postgres
- Port: 5432

These can be overridden with environment variables:
- PGHOST
- PGDATABASE
- PGUSER
- PGPASSWORD
- PGPORT
