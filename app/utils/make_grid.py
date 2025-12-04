import os
import sys
from PIL import Image
import argparse
import math # <-- Import math library for square root and ceiling

def create_smart_grid_collage(folder, output_filename, width=1200):
    """
    Creates a smart, compact grid collage that adapts to the number of images.
    """
    image_paths = sorted([os.path.join(folder, f) for f in os.listdir(folder) if f.lower().endswith(('.png', '.jpg', '.jpeg'))])
    
    num_images = len(image_paths)
    if num_images == 0:
        print("No images found in the folder.")
        return

    # --- SMART GRID CALCULATION ---
    # 1. Determine the number of columns (ideal grid is close to a square)
    columns = int(math.ceil(math.sqrt(num_images)))
    
    # 2. Calculate thumbnail size and number of rows
    thumb_size = width // columns
    rows = (num_images + columns - 1) // columns
    
    # 3. Calculate the final canvas dimensions (this is the key change)
    # The canvas will now fit the images perfectly (e.g., 1200x800 for 4-6 images)
    final_width = columns * thumb_size
    final_height = rows * thumb_size
    # --- END OF SMART CALCULATION ---

    # Create the blank canvas with the correct dimensions
    collage = Image.new('RGB', (final_width, final_height), (255, 255, 255))
    
    # Paste each image
    x, y = 0, 0
    for path in image_paths:
        try:
            with Image.open(path) as img:
                # Crop to square, resize, and paste (same as before)
                short_side = min(img.size)
                left = (img.width - short_side) / 2
                top = (img.height - short_side) / 2
                right = (img.width + short_side) / 2
                bottom = (img.height + short_side) / 2
                img = img.crop((left, top, right, bottom))
                img = img.resize((thumb_size, thumb_size))
                
                collage.paste(img, (x, y))
                
                # Move to the next grid position
                x += thumb_size
                if x >= final_width:
                    x = 0
                    y += thumb_size
        except Exception as e:
            print(f"Skipping file {path} due to error: {e}")
            
    collage.save(output_filename)
    print(f"Collage saved to {output_filename} with dimensions {final_width}x{final_height}")

if __name__ == '__main__':
    parser = argparse.ArgumentParser(description='Smart Grid Collage Maker')
    parser.add_argument('-f', '--folder', required=True, help='Folder with images')
    parser.add_argument('-o', '--output', required=True, help='Output collage filename')
    parser.add_argument('-w', '--width', type=int, default=1200, help='Maximum width of the collage')
    args = parser.parse_args()
    
    create_smart_grid_collage(args.folder, args.output, args.width)