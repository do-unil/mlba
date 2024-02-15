import os
from pdf2image import convert_from_path

def convert_pdf_images_to_png(source_folder, target_folder):
    """
    Converts all PDF images in the source_folder to PNG format and saves them in the target_folder.
    """
    for root, dirs, files in os.walk(source_folder):
        for file in files:
            if file.endswith('.pdf'):
                pdf_path = os.path.join(root, file)
                images = convert_from_path(pdf_path)
                for image in images:
                    base_filename = os.path.splitext(file)[0]
                    target_path = os.path.join(target_folder, base_filename + '.png')
                    image.save(target_path, 'PNG')
                    print(f"Converted and saved: {target_path}")

def update_tex_image_references(tex_folder, source_extension='.pdf', target_extension='.png'):
    """
    Updates all .tex files in tex_folder to reference images with the target_extension instead of the source_extension.
    """
    for root, dirs, files in os.walk(tex_folder):
        for file in files:
            if file.endswith('.tex'):
                tex_path = os.path.join(root, file)
                with open(tex_path, 'r', encoding='utf-8') as f:
                    content = f.read()
                
                # Replace the source extension with the target extension for image references
                updated_content = content.replace(source_extension, target_extension)
                
                with open(tex_path, 'w', encoding='utf-8') as f:
                    f.write(updated_content)
                print(f"Updated image references in: {tex_path}")

def main():
    current_directory = os.getcwd()
    # Assuming PDF images are located throughout the project directories
    source_folder = current_directory
    target_folder = os.path.join(current_directory, 'Graphs')
    
    # Ensure the target folder exists
    os.makedirs(target_folder, exist_ok=True)

    # Convert PDF images to PNG and place them in the "Graphs" folder
    convert_pdf_images_to_png(source_folder, target_folder)
    
    # Update .tex files throughout the project to reference the new PNG images
    update_tex_image_references(current_directory)

if __name__ == "__main__":
    main()
