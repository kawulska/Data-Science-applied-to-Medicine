
import json
import uuid
import random
from datetime import datetime, timedelta
import os

# Configuration
OUTPUT_DIR = "json_data"
os.makedirs(OUTPUT_DIR, exist_ok=True)

# Data pools
EXECUTION_NODES = ["cresselia", "darkrai", "giratina", "arceus", "palkia", "dialga",
                   "shaymin", "manaphy", "phione", "heatran"]

SAMPLE_PREFIXES = ["3D", "GS", "BT", "NK", "RT", "HX", "MZ", "PL", "VQ", "ZW"]
SAMPLE_SUFFIXES = ["_S1", "_S2", "_S3", "_S4", "_S5", "_S6", "_S7", "_S8"]
CATEGORIES = ["Genet", "Oncology", "Rare Disease", "Neurology", "Cardiology"]

SEQFU_VERSIONS = ["1.22.3", "1.22.1", "1.21.0", "1.20.5"]
SHA_VERSIONS = [
    "sha256sum (GNU coreutils) 8.32",
    "sha256sum (GNU coreutils) 8.30",
    "sha256sum (GNU coreutils) 9.0"
]
NEXTFLOW_COMMITS = ["a3f9c21", "b7d14e8", "c2a88f1", "N/A", "d5f0012"]

USERS = ["salle_alumni", "lab_tech_01", "genomics_admin", "researcher_mz", "pipeline_bot"]

SEQFU_STATUSES = ["OK", "FAIL", "WARN"]
SEQFU_WEIGHTS = [0.80, 0.12, 0.08]

SHA_PASS_TEMPLATES = [
    "{r1}: La suma coincide {r2}: La suma coincide ",
    "{r1}: OK {r2}: OK "
]
SHA_FAIL_TEMPLATES = [
    "{r1}: La suma NO coincide {r2}: La suma coincide ",
    "{r1}: MISMATCH {r2}: MISMATCH "
]

def random_sample_name():
    prefix = random.choice(SAMPLE_PREFIXES)
    number = random.randint(100, 999)
    suffix = random.choice(SAMPLE_SUFFIXES)
    return f"{prefix}_{number}{suffix}", f"{prefix}_ANVA_{number}{suffix}"

def random_start_time():
    base = datetime(2026, 3, 1, 8, 0, 0)
    offset_days = random.randint(0, 60)
    offset_hours = random.randint(0, 15)
    offset_mins = random.randint(0, 59)
    return base + timedelta(days=offset_days, hours=offset_hours, minutes=offset_mins)

def generate_record(index):
    activity_id = str(uuid.uuid4())
    sample_short, sample_long = random_sample_name()
    
    start_dt = random_start_time()
    duration_minutes = random.randint(2, 45)
    end_dt = start_dt + timedelta(minutes=duration_minutes)
    
    start_time = start_dt.strftime("%Y-%m-%dT%H:%M:%SZ")
    end_time = end_dt.strftime("%Y-%m-%dT%H:%M:%SZ")
    
    node = random.choice(EXECUTION_NODES)
    user = random.choice(USERS)
    category = random.choice(CATEGORIES)

    r1 = f"{sample_long}_R1_001.fastq.gz"
    r2 = f"{sample_long}_R2_001.fastq.gz"

    seqfu_status = random.choices(SEQFU_STATUSES, weights=SEQFU_WEIGHTS)[0]
    
    if seqfu_status == "OK":
        seqfu_val = f"OK PE {r1} 0 0 0 "
        sha_val = random.choice(SHA_PASS_TEMPLATES).format(r1=r1, r2=r2)
    elif seqfu_status == "FAIL":
        seqfu_val = f"FAIL PE {r1} 1 3 0 "
        sha_val = random.choice(SHA_FAIL_TEMPLATES).format(r1=r1, r2=r2)
    else:
        seqfu_val = f"WARN PE {r1} 0 1 0 "
        sha_val = random.choice(SHA_PASS_TEMPLATES).format(r1=r1, r2=r2)
    

    total_size_bytes = str(random.randint(1_000_000_000, 8_000_000_000))
    file_count = str(random.choice([2, 4, 6]))
    
    record = {
        "@context": "http://www.w3.org/ns/prov#",
        "@id": f"urn:uuid:{activity_id}",
        "@type": "Activity",
        "label": f"Processament complet de {sample_short}",
        "startTime": start_time,
        "endTime": end_time,
        "executionNode": node,
        "sourceDirectory": "/data/input/",
        "destinationDirectory": f"/data/output/{sample_short}",
        "wasAssociatedWith": [
            {
                "@type": "SoftwareAgent",
                "label": "seqfu",
                "version": random.choice(SEQFU_VERSIONS)
            },
            {
                "@type": "SoftwareAgent",
                "label": "sha256sum",
                "version": random.choice(SHA_VERSIONS)
            },
            {
                "@type": "SoftwareAgent",
                "label": "Pipeline Nextflow fastq_prov",
                "repository": "local",
                "commitId": random.choice(NEXTFLOW_COMMITS),
                "revision": "N/A"
            },
            {
                "@id": "urn:person:salle_alumni",
                "@type": "Person",
                "label": f"Usuari executor: {user}",
                "actedOnBehalfOf": {
                    "@id": "https://ror.org/01y990p52",
                    "@type": "Organization",
                    "label": "La Salle"
                }
            }
        ],
        "generated": [
            {
                "@type": "Entity",
                "label": "Verificació SHA256",
                "description": "Resultat de la comprovació de checksum a destí",
                "value": sha_val
            },
            {
                "@type": "Entity",
                "label": "Verificació Seqfu",
                "description": "Resultat de la comprovació d'integritat del format FASTQ",
                "value": seqfu_val
            },
            {
                "@type": "Entity",
                "label": "FASTQ Files",
                "totalSizeBytes": total_size_bytes,
                "category": category,
                "fileCount": file_count
            }
        ]
    }
    
    return record

def main():
    for i in range(1, 101):
        record = generate_record(i)
        filename = f"provenance_{i:03d}.json"
        filepath = os.path.join(OUTPUT_DIR, filename)
        with open(filepath, "w", encoding="utf-8") as f:
            json.dump(record, f, indent=2, ensure_ascii=False)

if __name__ == "__main__":
    main()
