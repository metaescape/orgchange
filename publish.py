import subprocess
import os
import sys
import shutil


theme = 'darkfloat'
orgfile = "/home/pipz/org/logical/zeromq.org"
target_folder = "/tmp"
current_file_dir = os.path.dirname(os.path.realpath(sys.argv[0]))

def export_to_html(orgfile, target_folder, theme):
    cmd = ['emacs', '--batch', f'--chdir={current_file_dir}/themes/{theme}', '--load', 'export.el', orgfile, '--eval', f"(progn (setq default-directory \"{target_folder}\") (org-html-export-to-html))", '--kill']
    # cmd = ['emacs', '--batch', f'--chdir={current_file_dir}/themes/{theme}', '--load', 'export.el', orgfile, '--eval', "(org-html-export-to-html)", '--kill']
    process = subprocess.Popen(cmd, stdout=subprocess.PIPE, stderr=subprocess.PIPE)
    output, error = process.communicate()
    
    html_path = orgfile.replace('.org', '.html')
    target_path = os.path.join(target_folder, os.path.basename(html_path))
    if not os.path.exists(target_path):
        print(f'failed on\n{" ".join(cmd)}')
    # if not os.path.exists(html_path):
    #     print(f'failed on\n{" ".join(cmd)}')
    # else:
    #     shutil.move(html_path, target_folder)


if __name__ == '__main__':
    export_to_html(orgfile, target_folder, theme)