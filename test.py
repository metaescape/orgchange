from pygments import highlight
from pygments.lexers import get_lexer_by_name
from pygments.formatters import HtmlFormatter

from pygments import highlight
from pygments.formatters import HtmlFormatter
import html
from bs4 import BeautifulSoup

code = 'def hello_world():\n    print("Hello, world!")'
escaped_code = html.escape(code)
escaped_code = code

lexer = get_lexer_by_name("python", stripall=True)
formatter = HtmlFormatter()

highlighted_code = highlight(escaped_code, lexer, formatter)

html_content = f"<html><body><pre>{highlighted_code}</pre></body></html>"
# print(html_content)

from dom import pygment_and_paren_match_all

html_doc = '<pre>你好<span class="p">(</span><span class="p">(</span><span class="p">(</span><span class="p">))):</span></pre>'
soup = BeautifulSoup(html_doc, "html.parser")


print(pygment_and_paren_match_all(soup).prettify())
