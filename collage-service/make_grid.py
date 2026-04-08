import os
from PIL import Image
import math

def create_adaptive_collage(image_paths: list, output_filename: str, canvas_width: int = 1200):
    num_images = len(image_paths)
    if num_images == 0:
        raise ValueError("No image paths provided.")

    # 1. Define the grid structure based on your sketch
    # Format: num_items -> [images_per_row_1, images_per_row_2, ...]
    layout_map = {
        1: [1],
        2: [2],
        3: [3],
        4: [2, 2],
        5: [3, 2],
        6: [3, 3],
        7: [3, 3, 1],
        8: [3, 3, 2],
        9: [3, 3, 3]
    }
    
    row_structure = layout_map.get(num_images, [3] * (num_images // 3) + ([num_images % 3] if num_images % 3 > 0 else []))
    num_rows = len(row_structure)

    # Calculate row height (we keep cells roughly square or slightly rectangular)
    # A standard 2x2 grid (4 items) has row_height = width // 2
    row_height = canvas_width // (max(row_structure) if num_images > 1 else 2)
    canvas_height = num_rows * row_height

    # 2. Create the blank canvas
    collage = Image.new('RGB', (canvas_width, canvas_height), (255, 255, 255))
    
    img_idx = 0
    current_y = 0
    
    for row_count in row_structure:
        # Calculate width of items in THIS specific row
        # This is what makes it adaptive (e.g. if row has 1 item, it takes 100% width)
        item_width = canvas_width // row_count
        current_x = 0
        
        for _ in range(row_count):
            if img_idx >= num_images: break
            
            try:
                with Image.open(image_paths[img_idx]) as img:
                    # Logic: Fill the cell while maintaining aspect ratio (Center Crop)
                    target_ratio = item_width / row_height
                    img_ratio = img.width / img.height
                    
                    if img_ratio > target_ratio:
                        # Image is too wide: crop left/right
                        new_width = int(img.height * target_ratio)
                        offset = (img.width - new_width) // 2
                        img = img.crop((offset, 0, offset + new_width, img.height))
                    else:
                        # Image is too tall: crop top/bottom
                        new_height = int(img.width / target_ratio)
                        offset = (img.height - new_height) // 2
                        img = img.crop((0, offset, img.width, offset + new_height))
                    
                    # Resize and Paste
                    img = img.resize((item_width, row_height), Image.Resampling.LANCZOS)
                    collage.paste(img, (current_x, current_y))
                    
                    current_x += item_width
                    img_idx += 1
            except Exception as e:
                print(f"Skipping damaged image: {e}")
                img_idx += 1
                
        current_y += row_height
            
    # 3. Save with optimization
    collage.save(output_filename, 'JPEG', quality=85, optimize=True)
    return output_filename