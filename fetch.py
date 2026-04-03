import urllib.request
import re

urls = {
    'Estimation': 'https://rpubs.com/tijahbus/L12_1',
    'Confidence_Intervals': 'https://rpubs.com/tijahbus/L12_2',
    'Bayesian': 'https://rpubs.com/tijahbus/L12_4',
    'Linear_models': 'https://rpubs.com/tijahbus/L14_1'
}

for name, url in urls.items():
    try:
        req = urllib.request.Request(url, headers={'User-Agent': 'Mozilla/5.0'})
        html = urllib.request.urlopen(req).read().decode('utf-8')
        match = re.search(r"<iframe[^>]+src=['\"](//rstudio-pubs-static[^'\"]+)['\"]", html)
        if match:
            iframe_url = 'https:' + match.group(1)
            print(f"Fetching {iframe_url} for {name}")
            iframe_req = urllib.request.Request(iframe_url, headers={'User-Agent': 'Mozilla/5.0'})
            iframe_html = urllib.request.urlopen(iframe_req).read()
            with open(f"{name}.html", 'wb') as f:
                f.write(iframe_html)
            print(f"Saved {name}.html")
        else:
            print(f"No iframe found for {name}")
    except Exception as e:
        print(f"Error processing {name}: {e}")
