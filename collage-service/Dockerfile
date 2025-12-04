FROM python:3.11-slim

WORKDIR /app

COPY requirements.txt .
RUN pip install -r requirements.txt

COPY . .

# Create the mount point for the shared data volume
RUN mkdir -p /data

CMD ["python3", "app.py"]