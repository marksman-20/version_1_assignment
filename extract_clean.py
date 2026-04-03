from bs4 import BeautifulSoup

modules = ['Estimation', 'Confidence_Intervals', 'Bayesian', 'Linear_models']
with open('problems_text.md', 'w', encoding='utf-8') as out:
    for module in modules:
        out.write(f"# Module: {module}\n\n")
        with open(f'{module}.html', 'r', encoding='utf-8') as f:
            soup = BeautifulSoup(f, 'html.parser')
            header = soup.find(lambda t: t.name in ['h1', 'h2', 'h3', 'h4'] and 'exercise' in t.text.lower())
            if not header:
                continue
            
            for sib in header.find_next_siblings():
                if sib.name in ['h1', 'h2', 'h3', 'h4'] and int(sib.name[1]) <= int(header.name[1]):
                    break
                
                # Replace imgs with their alt text showing it's math
                for img in sib.find_all('img'):
                    alt = img.get('alt', '')
                    img.replace_with(f" [MATH: {alt}] ")
                
                # Replace math elements
                for math in sib.find_all(class_='math'):
                    math.replace_with(f" ${math.text}$ ")
                
                out.write(sib.text.strip() + "\n\n")

print("Done")
