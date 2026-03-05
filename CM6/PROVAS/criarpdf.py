import os
import pypandoc
from pypdf import PdfWriter, PdfReader

def markdown_to_pdf_with_base(input_md, base_pdf, output_pdf):
    """
    Converte Markdown (com LaTeX) para PDF e aplica um PDF de fundo.
    """
    temp_content_pdf = "Material de Apoio em Branco (2025) (1).pdf"

    # 1. Configurar margens para o texto não bater no cabeçalho/rodapé do base
    # Ajuste os valores de 'top', 'bottom', 'left', 'right' conforme seu PDF base
    pandonc_args = [
        '-V', 'geometry:top=3cm',
        '-V', 'geometry:bottom=2cm',
        '-V', 'geometry:left=2cm',
        '-V', 'geometry:right=2cm',
        '--pdf-engine=pdflatex' # Garante suporte a LaTeX nativo
    ]

    print(f"Convertendo '{input_md}' para PDF temporário...")
    
    try:
        # Converte MD -> PDF mantendo suporte a fórmulas matemáticas ($..$)
        pypandoc.convert_file(
            input_md,
            'pdf',
            outputfile=temp_content_pdf,
            extra_args=pandonc_args
        )
    except OSError:
        print("ERRO: Pandoc ou LaTeX não encontrado. Verifique a instalação.")
        return

    # 2. Mesclar o conteúdo gerado com o PDF base
    print("Aplicando o PDF base...")
    
    writer = PdfWriter()
    
    # Abrir os arquivos
    reader_base = PdfReader(base_pdf)
    reader_content = PdfReader(temp_content_pdf)

    # Pegar a página de fundo (assumindo que o base tem apenas 1 página de modelo)
    # Se o base tiver várias páginas diferentes, a lógica precisaria ser adaptada.
    page_bg_template = reader_base.pages[0]

    for i in range(len(reader_content.pages)):
        content_page = reader_content.pages[i]
        
        # Criar uma cópia nova da base para esta página
        # (Isso é crucial, senão ele modifica a referência original)
        current_base_page = reader_base.pages[0] # Recarrega a ref ou clona se necessário
        
        # O método merge_page coloca o argumento (content_page) POR CIMA do objeto (current_base_page)
        # É necessário criar um PageObject novo para não acumular conteúdo
        
        # TRUQUE DO PYPDF: Para repetir o fundo, o ideal é criar uma página em branco,
        # mesclar o fundo nela, e depois mesclar o conteúdo.
        
        writer.add_page(current_base_page)
        # Pegamos a última página adicionada (que agora é uma cópia independente no writer)
        last_page = writer.pages[-1]
        last_page.merge_page(content_page)

    # 3. Salvar o arquivo final
    with open(output_pdf, "wb") as f:
        writer.write(f)

    # Limpeza
    if os.path.exists(temp_content_pdf):
        os.remove(temp_content_pdf)

    print(f"Sucesso! Arquivo gerado: {output_pdf}")

# --- EXECUTANDO O CÓDIGO ---

# Cria um arquivo markdown de exemplo para teste
exemplo_md = """
# Relatório de Física

Este texto foi gerado a partir de um arquivo Markdown. O fundo é um PDF separado.

## Equações com LaTeX

Aqui está uma fórmula de exemplo renderizada via LaTeX:

$$ E = mc^2 $$

Ou a fórmula de Bhaskara:

$$ x = \\frac{-b \\pm \\sqrt{b^2 - 4ac}}{2a} $$

## Lista de Itens

* Item 1
* Item 2
* Item 3
"""

# Salva o MD de exemplo
with open("exemplo.md", "w", encoding="utf-8") as f:
    f.write(exemplo_md)

# Certifique-se de ter um arquivo 'base.pdf' na mesma pasta.
# Se não tiver, o script vai falhar na leitura do base.
# markdown_to_pdf_with_base("exemplo.md", "fundo.pdf", "relatorio_final.pdf")

print("Para testar, coloque um arquivo 'fundo.pdf' na pasta e descomente a última linha do código.")