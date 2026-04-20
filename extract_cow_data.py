import pdfplumber
import pandas as pd
import os
import re

def clean_sheet_name(name):
    # Remove invalid characters for Excel sheet names
    name = re.sub(r'[\\/*?:\[\]]', '', name)
    return name[:31]

def extract_data_from_pdf(pdf_path):
    print(f"Processing: {pdf_path}")
    data = []
    headers = []
    report_title = os.path.basename(pdf_path).replace(".pdf", "")
    
    try:
        with pdfplumber.open(pdf_path) as pdf:
            for page_num, page in enumerate(pdf.pages):
                text = page.extract_text()
                if not text:
                    continue
                
                lines = text.split("\n")
                if page_num == 0:
                    # Try to get the title from the first few lines
                    if len(lines) > 1:
                        report_title = lines[1].strip()
                
                # Identify where data starts. 
                # In most DairyPlan reports, data rows start with a number (Vaca N°)
                # and are preceded by headers that often contain "---" or specific keywords.
                
                table_started = False
                for line in lines:
                    line = line.strip()
                    if not line:
                        continue
                    
                    # Keywords that suggest header lines or irrelevant info
                    if "DairyPlan" in line or "Pag" in line or "Pág" in line:
                        continue
                    
                    # Check if it's a data row (starts with a digit, e.g., 3478)
                    if re.match(r'^\d+', line):
                        table_started = True
                        # Split by space, but handle the "____" or long comments at the end
                        parts = line.split()
                        data.append(parts)
                    elif not table_started:
                        # Before the table starts, we might be collecting headers
                        # We'll just store them for now, maybe as a single line
                        if "---" in line or any(k in line.lower() for k in ["n°", "vaca", "animal", "gp"]):
                            headers.append(line)
                            
    except Exception as e:
        print(f"Error processing {pdf_path}: {e}")
        
    if not data:
        return None, None
    
    # Try to determine the max number of columns in the data
    max_cols = max(len(row) for row in data)
    
    # Create a DataFrame
    df = pd.DataFrame(data)
    
    # Simple header clean up: use the last header line found or generic names
    if headers:
        header_line = headers[-1]
        raw_cols = header_line.split()
        # If the number of header parts matches or is close, use them
        if len(raw_cols) == max_cols:
            df.columns = raw_cols
        else:
            # Generic columns if mismatch
            df.columns = [f"Col_{i}" for i in range(df.shape[1])]
    else:
        df.columns = [f"Col_{i}" for i in range(df.shape[1])]
        
    return report_title, df

def main():
    pdf_files = [f for f in os.listdir(".") if f.endswith(".pdf")]
    if not pdf_files:
        print("No PDF files found.")
        return

    output_file = "Datos_Vacas_Consolidado.xlsx"
    with pd.ExcelWriter(output_file, engine='openpyxl') as writer:
        for pdf_file in pdf_files:
            title, df = extract_data_from_pdf(pdf_file)
            if df is not None:
                sheet_name = clean_sheet_name(title)
                # Ensure sheet name is unique
                if sheet_name in writer.sheets:
                    sheet_name = (sheet_name[:25] + "_" + str(len(writer.sheets)))[:31]
                df.to_excel(writer, sheet_name=sheet_name, index=False)
                print(f"Added sheet: {sheet_name} ({len(df)} rows)")

    print(f"\nExtraction complete! Data saved to {output_file}")

if __name__ == "__main__":
    main()
