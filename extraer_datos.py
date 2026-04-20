"""
Script de extracción y normalización de datos
Establo Sausalito - DairyPlan PDF Parser
Ejecutar: python extraer_datos.py --input ./pdfs/ --output ./
"""

import subprocess, re, os, pandas as pd, argparse

def read_pdf(fname):
    result = subprocess.run(['pdftotext', '-layout', fname, '-'], capture_output=True, text=True)
    return result.stdout

def parse_produccion(text):
    rows = []
    for line in text.split('\n'):
        m = re.match(r'\s{3,}(\d{4})\s+(\d+)\s+(\w+\d*)\s+(\d+)\s+\d+:\d+\s+\d{2}-\d{2}\s+([\d.]+)\s+([\d.]+)\s+([\d.]+)\s+(-?[\d.]+)\s+([\d.]+)\s+(\d+)\s+(\d+)', line)
        if m:
            rows.append({'vaca_no': int(m.group(1)), 'grupo': int(m.group(2)), 'status': m.group(3),
                         'dim': int(m.group(4)), 'prod_ultimo': float(m.group(5)),
                         'prod_prom_act': float(m.group(6)), 'prod_prom_ant': float(m.group(7)),
                         'variacion': float(m.group(8)), 'max_prod': float(m.group(9)),
                         'dia_max': int(m.group(10)), 'total_prod': int(m.group(11))})
    return pd.DataFrame(rows)

def parse_gestantes(text):
    rows = []
    for line in text.split('\n'):
        m = re.match(r'\s+(\d{4})\s+\d+\s+(\d+)\s+(\d{2}-\d{2}-\d{2})\s+(\d+)\s+(\w+)\s+(\d+)\s+\S*\s+(\d+)\s+(\d+)\s+([\d.]+)', line)
        if m:
            rows.append({'vaca_no': int(m.group(1)), 'lactacion': int(m.group(2)),
                         'fecha_parto': m.group(3), 'dim': int(m.group(4)),
                         'status': m.group(5), 'dias_gest': int(m.group(6)),
                         'num_insem': int(m.group(7)), 'dias_abierta': int(m.group(8)),
                         'prod_prom': float(m.group(9))})
    return pd.DataFrame(rows)

def parse_para_secar(text):
    rows = []
    for line in text.split('\n'):
        m = re.match(r'\s+(\d{4})\s+(\d+)\s+(\w+\d*)\s+(\d+)\s+(-?\d+)\s+(-?\d+)\s+([\d.]+)\s+(\S+)\s+(\S+)', line)
        if m:
            rows.append({'vaca_no': int(m.group(1)), 'grupo': int(m.group(2)), 'status': m.group(3),
                         'dim': int(m.group(4)), 'dias_al_secado': int(m.group(5)),
                         'dias_al_parto': int(m.group(6)), 'prod_prom': float(m.group(7)),
                         'fecha_secado': m.group(8), 'fecha_parto_est': m.group(9)})
    return pd.DataFrame(rows)

def parse_proximas_parto(text):
    rows = []
    for line in text.split('\n'):
        parts = line.split()
        if len(parts) >= 10 and re.match(r'^\d{4}$', parts[0]) and re.match(r'^\d+$', parts[1]):
            try:
                fecha_idx = next(i for i,p in enumerate(parts) if re.match(r'\d{2}-\d{2}-\d{2}', p) and i > 5)
                rows.append({'vaca_no': int(parts[0]), 'grupo': int(parts[1]),
                             'dias_faltan': int(parts[-2]), 'status': parts[-1],
                             'fecha_parto_est': parts[fecha_idx]})
            except: pass
    return pd.DataFrame(rows)

def parse_inseminadas(text):
    rows = []
    for line in text.split('\n'):
        m = re.match(r'\s+(\d{4})\s+(\d+)\s+(\d+)\s+\S+\s+(\w+)\s+(\S+)\s+(\S+)\s+T3\s+(\d+)\s+(\d+)\s+(\d+)\s+([\d.]+)', line)
        if m:
            rows.append({'vaca_no': int(m.group(1)), 'grupo': int(m.group(2)),
                         'lactacion': int(m.group(3)), 'status': m.group(4),
                         'fecha_insem': m.group(5), 'toro': m.group(6),
                         'num_insem': int(m.group(7)), 'dias_insem': int(m.group(8)),
                         'dias_abierta': int(m.group(9)), 'prod_prom': float(m.group(10))})
    return pd.DataFrame(rows)

def parse_secas(text):
    rows = []
    for line in text.split('\n'):
        m = re.match(r'\s+(\d{4})\s+(\d+)\s+(\w+)\s+\S+\s+(\d+)\s+(\d+)\s+(\d+)', line)
        if m:
            rows.append({'vaca_no': int(m.group(1)), 'grupo': int(m.group(2)),
                         'status': m.group(3), 'dias_secada': int(m.group(4)),
                         'lactacion_num': int(m.group(5)), 'dim_prev': int(m.group(6))})
    return pd.DataFrame(rows)

def parse_problema(text):
    rows = []
    for line in text.split('\n'):
        m = re.match(r'\s+(\d{4})\s+(Insem|Matar|Abort|Preñ)\s+(\d+)\s+(\d+)', line)
        if m:
            rows.append({'vaca_no': int(m.group(1)), 'status': m.group(2),
                         'grupo': int(m.group(3)), 'dim': int(m.group(4))})
    return pd.DataFrame(rows)

def parse_mastitis(text):
    rows = []
    for line in text.split('\n'):
        m = re.match(r'\s+(\d{4})\s+([\d.]+)\s+(\d+)\s+(\d+)\s+([\d.]+)\s+(\*+|-?\d+)\s+(\S+)', line)
        if m:
            rows.append({'vaca_no': int(m.group(1)), 'edad': float(m.group(2)),
                         'lactacion': int(m.group(3)), 'dim': int(m.group(4)),
                         'prod_24h': float(m.group(5))})
    return pd.DataFrame(rows)

def parse_test_prenez(text):
    rows = []
    for line in text.split('\n'):
        m = re.match(r'\s+(\d{4})\s+\d+\s+(\w+)\s+(\d+)\s+(\d+)\s+(\d+)\s+(\d+)\s+(\S+)\s+(\d+)\s+(\d+)', line)
        if m:
            rows.append({'vaca_no': int(m.group(1)), 'status': m.group(2),
                         'grupo': int(m.group(3)), 'lactacion': int(m.group(4)),
                         'dim': int(m.group(5)), 'num_insem': int(m.group(6)),
                         'fecha_insem': m.group(7), 'dias_insem': int(m.group(8)),
                         'iep': int(m.group(9))})
    return pd.DataFrame(rows)

def main():
    parser = argparse.ArgumentParser()
    parser.add_argument('--input', default='.', help='Carpeta con los PDFs')
    parser.add_argument('--output', default='.', help='Carpeta de salida para CSVs')
    args = parser.parse_args()

    os.makedirs(args.output, exist_ok=True)
    path = args.input

    archivos = {
        'PRODUCCI': ('produccion.csv', parse_produccion),
        'VACAS GESTANTES': ('gestantes.csv', parse_gestantes),
        'VACAS PARA SECAR': ('para_secar.csv', parse_para_secar),
        'VACAS PROXIMAS AL PARTO': ('proximas_parto.csv', parse_proximas_parto),
        'VACAS INSEMINADAS': ('inseminadas.csv', parse_inseminadas),
        'VACAS SECAS': ('secas.csv', parse_secas),
        'VACAS PROBLEMA': ('problema.csv', parse_problema),
        'MASTITIS': ('mastitis.csv', parse_mastitis),
        'TEST DE PRE': ('test_prenez.csv', parse_test_prenez),
    }

    for fname in os.listdir(path):
        if not fname.endswith('.pdf'): continue
        clean = fname.upper()
        for key, (outname, parser_fn) in archivos.items():
            if key in clean:
                text = read_pdf(os.path.join(path, fname))
                df = parser_fn(text)
                out_path = os.path.join(args.output, outname)
                df.to_csv(out_path, index=False)
                print(f"✅ {outname}: {len(df)} registros")
                break

    print("\n✅ Extracción completada.")

if __name__ == '__main__':
    main()
