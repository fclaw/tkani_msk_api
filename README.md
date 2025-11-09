**What to add to this folder:**

* A `requirements.txt` file containing the line `python-telegram-bot`.
* Your `bot.py` script.

---

### 2. `README.md` for `tkani-api`

This README should be more technical. It describes the "brain" of the operation, its purpose, its API endpoints, and how to set it up.

**Create this file at:** `tkani/tkani-api/README.md`

```markdown
# Tkani API Service

This is the back-end service for the "Tkani MSK" Telegram channel. It handles all core business logic, database interactions, and provides a RESTful API for client applications (such as the `transactional_bot`).

The service is built using **[Haskell/C++]** with a PostgreSQL database.

---

## 🏛️ Architecture

This service is the "Model" and "Controller" in an MVC-like architecture. It is responsible for:
*   Managing the state of customer orders.
*   Storing and retrieving product and customer information from the database.
*   Integrating with third-party services (e.g., payment gateways).
*   Providing a stable API for any front-end client.

---

##  API Endpoints

The API includes Swagger/OpenAPI documentation, which can be viewed at the `/swagger-ui` endpoint when the server is running.

Key endpoints include:

*   `GET /product/{product_id}`: Retrieves details for a specific product.
*   `POST /order`: Initiates a new order for a user.
*   `POST /order/{order_id}/address`: Adds a shipping address to an order.
*   `POST /order/{order_id}/phone`: Adds a contact phone number to an order.
*   `POST /order/{order_id}/finalize`: Confirms the order and triggers payment/admin notifications.

---

## 🛠️ Setup and Installation

### Prerequisites

*   **[GHC & Stack / C++ Compiler & CMake]**
*   PostgreSQL server running.
*   A database named `tkani_db` with the correct schema (see `schema.sql`).

### Database Setup

1.  **Create the database:**
    ```sql
    CREATE DATABASE tkani_db;
    ```

2.  **Run the schema migration:**
    ```bash
    psql -U youruser -d tkani_db < schema.sql
    ```

### Running the Service

1.  **Configure the application:**
    Copy `config.example.json` to `config.json` and fill in your database connection details.

2.  **Build and run the project:**

    *   **Using Stack:**
        ```bash
        stack build
        stack exec tkani-api-exe
        ```
    *   **Using CMake:**
        ```bash
        mkdir build && cd build
        cmake ..
        make
        ./tkani_backend
        ```

3.  The service will start on `http://localhost:8080`.

---
```
