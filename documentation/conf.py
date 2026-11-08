# Configuration file for the Gaur user guide.
project = "Gaur"
html_title = "Gaur"

extensions = [
    "myst_parser",
    "sphinx_design",
    "sphinx_copybutton",
    "sphinxcontrib.mermaid",
]

# Never treat these as documentation sources
exclude_patterns = [
    "_build",
    ".venv",
    "venv",
    "Thumbs.db",
    ".DS_Store",
    "README.md",
]

# Markdown (MyST)
source_suffix = {
    ".md": "markdown",
}
root_doc = "index"
myst_enable_extensions = ["colon_fence", "attrs_inline"]
myst_fence_as_directive = ["mermaid"]

# Theme
html_theme = "furo"
html_static_path = ["_static"]
html_css_files = ["custom.css"]
html_baseurl = "https://verify-at-iitpkd.gitlab.io/gaur/guide/"

html_theme_options = {
    "source_repository": "https://gitlab.com/verify-at-iitpkd/gaur/",
    "source_branch": "main",
    "source_directory": "documentation/",
}