import os
import subprocess
import re

def preprocess_tex_file(tex_path):
    """
    Reads a .tex file, removes the first occurrence of a specific \input command,
    truncates content after \end{document}, and processes \include and \input commands
    for the contents of the referenced files. Assumes relative paths for included files.
    """
    with open(tex_path, 'r', encoding='utf-8') as file:
        lines = file.readlines()
    
    # Check if the first line is the specific \input command and remove it
    if lines and lines[0].strip() == '\\input{../Main/ML_Main.tex}':
        lines = lines[1:]  # Remove the first line
    
    content = ''.join(lines)
    
    # Truncate content after \end{document}
    end_document_index = content.find('\\end{document}')
    if end_document_index != -1:
        content = content[:end_document_index + len('\\end{document}')]

    pattern = re.compile(r'\\(include|input)\{([^\}]+)\}')
    
    def replace_include(match):
        command, include_path = match.groups()
        include_file_path = os.path.join(os.path.dirname(tex_path), f'{include_path}.tex' if not include_path.endswith('.tex') else include_path)
        try:
            with open(include_file_path, 'r', encoding='utf-8') as include_file:
                return include_file.read()
        except FileNotFoundError:
            print(f"Warning: Included file {include_file_path} not found.")
            return ''
    
    content = pattern.sub(replace_include, content)
    
    preprocessed_path = tex_path + '.preprocessed'
    with open(preprocessed_path, 'w', encoding='utf-8') as file:
        file.write(content)
    
    return preprocessed_path


def clean_up_headers_and_merge_sections(md_content):
    """
    Removes the "### Table of Contents" header and merges sections with identical titles.
    """
    content = re.sub(r'### Table of Contents\s*\n+', '', md_content)
    headers = re.findall(r'^(### .+)$', content, flags=re.MULTILINE)
    
    for header in set(headers):
        if headers.count(header) > 1:
            pattern = re.compile(r'^' + re.escape(header) + r'\n+', flags=re.MULTILINE)
            content = pattern.sub('', content, count=(headers.count(header) - 1))
    
    return content

def postprocess_markdown(content):
    """
    Post-processes the Markdown content to handle specific LaTeX remnants, image captions,
    and ensure all images are center-aligned with fig-align="center".
    """
    # Replace or remove \A, depending on its intended use. Example shown is for removal.
    content = content.replace('\\A', 'A')
    
    # Process image captions, leaving them empty, and ensure images are center-aligned
    content = re.sub(r'!\[image\]\((.*?)\)\{width="([^"]*)"\}', 
                     r'![](\1){width="\2" fig-align="center"}', content)
    
    return content


def convert_tex_to_qmd(tex_path, title, ignore_dirs):
    """
    Converts a LaTeX file to a Quarto-compatible Markdown file (.qmd),
    including preprocessing and post-processing for headers and sections.
    """
    if any(ignored_dir in tex_path for ignored_dir in ignore_dirs):
        print(f"Ignoring file in {tex_path} as it's in an ignored directory.")
        return

    preprocessed_tex_path = preprocess_tex_file(tex_path)
    qmd_path = tex_path.replace('.tex', '.qmd')
    title = title.replace("ML_", "")
    
    subprocess.run(['pandoc', '-f', 'latex', '-t', 'markdown', '-o', qmd_path, preprocessed_tex_path])
    
    if os.path.exists(qmd_path):
        with open(qmd_path, 'r', encoding='utf-8') as file:
            md_content = file.read()
        
        # Apply post-processing to handle specific LaTeX remnants and image captions
        md_content = postprocess_markdown(md_content)
        
        md_content = clean_up_headers_and_merge_sections(md_content)
        
        yaml_header = f"""---
title: {title}
format: html
execute:
  freeze: auto
  output: true
code-tools: false
code-fold: false
code-link: false
---
"""
        with open(qmd_path, 'w', encoding='utf-8') as file:
            file.write(yaml_header + md_content)
    else:
        print(f"Conversion failed for {tex_path}.")

def remove_unnecessary_files(directory, ignore_dirs):
    """
    Removes all files in the specified directory (and subdirectories) that are not .pdf, .tex, .qmd, or .py files,
    while ignoring specific directories.
    """
    for root, dirs, files in os.walk(directory, topdown=True):
        # If the current root is within any of the ignore_dirs, skip processing its files
        if any(os.path.relpath(root, start=directory).startswith(os.path.relpath(ignore_dir, start=directory)) for ignore_dir in ignore_dirs):
            continue  # Skip the entire ignored directory

        for file in files:
            if not file.endswith(('.pdf', '.tex', '.qmd', '.py')):
                file_path = os.path.join(root, file)
                try:
                    os.remove(file_path)
                    print(f"Removed: {file_path}")
                except Exception as e:
                    print(f"Error removing {file_path}: {e}")

def main():
    # Define the directories to ignore during the cleanup process
    ignore_dirs = [os.path.join(os.getcwd(), 'Graphs')]  # Full pat
    for root, dirs, files in os.walk("."):
        for file in files:
            if file.endswith('.tex') and not any(ignored_dir in os.path.join(root, file) for ignored_dir in ignore_dirs):
                tex_path = os.path.join(root, file)
                title = os.path.splitext(os.path.basename(file))[0]
                convert_tex_to_qmd(tex_path, title, ignore_dirs)

    remove_unnecessary_files(".", ignore_dirs)
    print("Conversion complete.")

if __name__ == "__main__":
    main()
