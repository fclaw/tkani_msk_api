# collage-service/app.py

import os
from flask import Flask, request, jsonify

# Import our new collage creation function
from make_grid import create_collage

app = Flask(__name__)

# This is the path INSIDE the container where the shared volume is mounted
SHARED_VOLUME_PATH = "/data"

@app.route('/generate-collage', methods=['POST'])
def generate_collage_endpoint():
    # 1. Get data from the Haskell API's request
    try:
        data = request.get_json(force=True)
        if not data:
            raise ValueError("Invalid or empty JSON payload.")
        
        image_filenames = data.get('image_paths') # e.g., ["img1.jpg", "img2.jpg"]
        output_filename = data.get('output_filename')
        width = int(data.get('width', 1200))

        if not image_filenames or not output_filename:
            raise ValueError("Missing 'image_paths' or 'output_filename'.")

    except Exception as e:
        return jsonify({"ok": False, "error": f"Invalid request: {e}"}), 400

    # 2. Construct absolute file paths inside the container's volume
    full_input_paths = [os.path.join(SHARED_VOLUME_PATH, fname) for fname in image_filenames]
    full_output_path = os.path.join(SHARED_VOLUME_PATH, output_filename)

    # 3. Call the core collage logic
    try:
        create_collage(
            image_paths=full_input_paths, 
            output_filename=full_output_path, 
            width=width
        )
        
        # 4. Return success response to the Haskell API
        return jsonify({
            "ok": True,
            # The Haskell API only needs the relative filename back
            "result_path": output_filename
        })

    except Exception as e:
        # If create_collage fails (e.g., can't open an image)
        return jsonify({"ok": False, "error": str(e)}), 500

if __name__ == '__main__':
    # Using 'debug=True' is fine for development
    app.run(host='0.0.0.0', port=5001, debug=True)