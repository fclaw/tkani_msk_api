# collage-service/make_grid.py

import os
from PIL import Image
import math

def create_collage(image_paths: list, output_filename: str, width: int = 1200):
    """
    Creates a smart, compact grid collage from a given list of image paths.
    """
    num_images = len(image_paths)
    if num_images == 0:
        raise ValueError("No image paths provided.")

    # --- Smart Grid Calculation ---
    columns = int(math.ceil(math.sqrt(num_images)))
    thumb_size = width // columns
    rows = (num_images + columns - 1) // columns
    
    final_width = columns * thumb_size
    final_height = rows * thumb_size
    # --- End of Calculation ---

    # Create the blank canvas
    collage = Image.new('RGB', (final_width, final_height), (255, 255, 255))
    
    x, y = 0, 0
    for path in image_paths:
        try:
            with Image.open(path) as img:
                # Crop image to a square from the center
                short_side = min(img.size)
                left = (img.width - short_side) / 2
                top = (img.height - short_side) / 2
                right = (img.width + short_side) / 2
                bottom = (img.height + short_side) / 2
                img = img.crop((left, top, right, bottom))
                
                # Resize and paste
                img = img.resize((thumb_size, thumb_size), Image.Resampling.LANCZOS)
                collage.paste(img, (x, y))
                
                # Move to the next grid position
                x += thumb_size
                if x >= final_width:
                    x = 0
                    y += thumb_size
        except Exception as e:
            # Propagate the error up to the web service
            raise IOError(f"Failed to process image {path}: {e}")
            
    # Save the final collage
    collage.save(output_filename, 'JPEG', quality=85, optimize=True)
    print(f"Collage saved to {output_filename}") # Good for logging
    return output_filename