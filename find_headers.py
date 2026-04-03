from bs4 import BeautifulSoup

for filename in ['Estimation.html', 'Confidence_Intervals.html', 'Bayesian.html', 'Linear_models.html']:
    with open(filename, 'r', encoding='utf-8') as f:
        soup = BeautifulSoup(f, 'html.parser')
        headers = soup.find_all(['h1', 'h2', 'h3', 'h4'])
        print(f"--- {filename} Headers ---")
        for h in headers[-5:]: # Print last 5 headers
            print(h.text.strip())
