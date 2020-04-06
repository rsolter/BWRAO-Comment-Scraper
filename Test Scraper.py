
# For downloading web pages
import requests
from bs4 import BeautifulSoup

page = requests.get("https://www.blackwhitereadallover.com/2020/4/3/21206623/miralem-pjanic-marco-verratti-juventus-paris-saint-germain-2020-serie-a-summer-transfer-rumors#comments")


soup = BeautifulSoup(page.content, 'html.parser')
print(soup.prettify())
list(soup.children)
