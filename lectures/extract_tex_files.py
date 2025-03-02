import os
import shutil
import argparse

def find_cours_ml_dir():
    """
    Find the cours_ml directory by walking up from the current directory
    """
    # Start from current directory
    current_dir = os.getcwd()
    
    # Check if current directory is cours_ml
    if os.path.basename(current_dir) == 'cours_ml':
        return current_dir
    
    # Look for cours_ml in current directory
    if 'cours_ml' in os.listdir(current_dir):
        return os.path.join(current_dir, 'cours_ml')
    
    # Walk up the directory tree
    parent_dir = os.path.dirname(current_dir)
    while parent_dir != current_dir:  # Stop at filesystem root
        if os.path.basename(parent_dir) == 'cours_ml':
            return parent_dir
        
        if 'cours_ml' in os.listdir(parent_dir):
            return os.path.join(parent_dir, 'cours_ml')
        
        current_dir = parent_dir
        parent_dir = os.path.dirname(current_dir)
    
    return None

def extract_tex_files(source_dir, target_dir):
    """
    Extract all .tex files from source_dir and its subdirectories
    and copy them to target_dir, appending only the immediate parent directory name
    """
    if not os.path.exists(target_dir):
        os.makedirs(target_dir)
        print(f"Created directory: {target_dir}")
    
    count = 0
    # Keep track of files to handle duplicates
    seen_files = {}
    
    for root, dirs, files in os.walk(source_dir):
        for file in files:
            if file.endswith('.tex'):
                source_path = os.path.join(root, file)
                
                # Get the immediate parent directory name
                parent_dir_name = os.path.basename(os.path.dirname(source_path))
                
                # Create new filename with parent dir prepended
                if parent_dir_name != os.path.basename(source_dir):  # Don't append if it's the root source dir
                    # new_filename = f"{parent_dir_name}_{file}"
                    new_filename = file
                else:
                    new_filename = file
                
                target_path = os.path.join(target_dir, new_filename)
                
                # Handle duplicates by appending a counter
                if new_filename in seen_files:
                    seen_files[new_filename] += 1
                    name, ext = os.path.splitext(new_filename)
                    target_path = os.path.join(target_dir, f"{name}_{seen_files[new_filename]}{ext}")
                else:
                    seen_files[new_filename] = 0
                
                shutil.copy2(source_path, target_path)
                print(f"Copied: {source_path} → {target_path}")
                count += 1
    
    return count

def main():
    # Find the cours_ml directory
    cours_ml_dir = find_cours_ml_dir()
    
    if not cours_ml_dir:
        print("Error: Could not find 'cours_ml' directory")
        return
    
    print(f"Found cours_ml directory at: {cours_ml_dir}")
    
    # Create the target directory
    # Place it at the same level as cours_ml
    parent_dir = os.path.dirname(cours_ml_dir)
    target_dir = os.path.join(parent_dir, 'cours_ml_extracted')
    
    count = extract_tex_files(cours_ml_dir, target_dir)
    print(f"\nExtraction complete: {count} .tex files have been copied to {target_dir}")

if __name__ == "__main__":
    main() 