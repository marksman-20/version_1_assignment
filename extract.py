from bs4 import BeautifulSoup
import markdownify

modules = ['Estimation', 'Confidence_Intervals', 'Bayesian', 'Linear_models']
with open('problems.md', 'w', encoding='utf-8') as out:
    for module in modules:
        out.write(f"# Module: {module}\n\n")
        with open(f'{module}.html', 'r', encoding='utf-8') as f:
            soup = BeautifulSoup(f, 'html.parser')
            header = soup.find(lambda t: t.name in ['h1', 'h2', 'h3', 'h4'] and 'exercise' in t.text.lower())
            if not header:
                print(f"Exercises not found in {module}")
                continue
            
            content = []
            for sib in header.find_next_siblings():
                if sib.name in ['h1', 'h2', 'h3', 'h4'] and int(sib.name[1]) <= int(header.name[1]):
                    break
                content.append(str(sib))
            
            md_text = markdownify.markdownify(''.join(content))
            out.write(md_text.strip() + "\n\n")

print("Finished extracting problems to problems.md")
