import re
from bs4 import BeautifulSoup
import base64
import os

modules = ['Estimation', 'Confidence_Intervals', 'Bayesian', 'Linear_models']

for module in modules:
    with open(f'{module}.html', 'r', encoding='utf-8') as f:
        soup = BeautifulSoup(f, 'html.parser')
        header = soup.find(lambda t: t.name in ['h1', 'h2', 'h3', 'h4'] and 'exercise' in t.text.lower())
        if not header:
            continue
        
        img_count = 1
        for sib in header.find_next_siblings():
            if sib.name in ['h1', 'h2', 'h3', 'h4'] and int(sib.name[1]) <= int(header.name[1]):
                break
            
            for img in sib.find_all('img'):
                src = img.get('src', '')
                if src.startswith('data:image/png;base64,'):
                    b64_data = src.split(',')[1]
                    img_data = base64.b64decode(b64_data)
                    filename = f"{module}_prob_{img_count}.png"
                    with open(filename, 'wb') as out_img:
                        out_img.write(img_data)
                    print(f"Saved {filename}")
                    img_count += 1
