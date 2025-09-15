import pandas as pd
import argparse
import gzip

def parse_vcf(input_vcf, output_file):
    # VEP headers as provided
    vep_headers = [
        "Allele", "Consequence", "IMPACT", "SYMBOL", "Gene", "Feature_type", "Feature", "BIOTYPE",
        "EXON", "INTRON", "HGVSc", "HGVSp", "cDNA_position", "CDS_position", "Protein_position",
        "Amino_acids", "Codons", "Existing_variation", "DISTANCE", "STRAND", "FLAGS", "VARIANT_CLASS",
        "SYMBOL_SOURCE", "HGNC_ID", "CANONICAL", "MANE_SELECT", "MANE_PLUS_CLINICAL", "TSL", "APPRIS",
        "CCDS", "ENSP", "SWISSPROT", "TREMBL", "UNIPARC", "UNIPROT_ISOFORM", "SOURCE", "GENE_PHENO",
        "DOMAINS", "miRNA", "HGVS_OFFSET", "AF", "AFR_AF", "AMR_AF", "EAS_AF", "EUR_AF", "SAS_AF",
        "gnomADe_AF", "gnomADe_AFR_AF", "gnomADe_AMR_AF", "gnomADe_ASJ_AF", "gnomADe_EAS_AF",
        "gnomADe_FIN_AF", "gnomADe_NFE_AF", "gnomADe_OTH_AF", "gnomADe_SAS_AF", "gnomADg_AF",
        "gnomADg_AFR_AF", "gnomADg_AMI_AF", "gnomADg_AMR_AF", "gnomADg_ASJ_AF", "gnomADg_EAS_AF",
        "gnomADg_FIN_AF", "gnomADg_MID_AF", "gnomADg_NFE_AF", "gnomADg_OTH_AF", "gnomADg_SAS_AF",
        "MAX_AF", "MAX_AF_POPS", "FREQS", "CLIN_SIG", "SOMATIC", "PHENO", "PUBMED", "MOTIF_NAME",
        "MOTIF_POS", "HIGH_INF_POS", "MOTIF_SCORE_CHANGE", "TRANSCRIPTION_FACTORS"
    ]

    # Fields to extract from the INFO column
    info_fields = ["AF", "AQ", "AC", "AN"]

    # Open VCF (handle gzipped or plain text)
    if input_vcf.endswith(".gz"):
        open_func = gzip.open
        mode = "rt"  # text mode
    else:
        open_func = open
        mode = "r"

    with open_func(input_vcf, mode) as f:
        lines = f.readlines()

    # Filter out metadata lines
    metadata_lines = [line for line in lines if line.startswith('#')]
    vcf_data = [line.strip() for line in lines if not line.startswith('#')]

    # Extract sample column names from the VCF header
    headers = metadata_lines[-1].strip().split('\t')
    sample_columns = headers[9:]  # Sample names start at the 10th column

    # Initialize a list for parsed rows
    parsed_rows = []

    for row in vcf_data:
        fields = row.split('\t')

        if len(fields) < 9:
            raise ValueError(f"Invalid VCF row format. Expected at least 9 columns, got {len(fields)}: {row}")

        # Extract fixed fields
        chrom, pos, id, ref, alt, qual, filt, info, fmt = fields[:9]
        sample_data = fields[9:]  # Sample genotype data

        # Parse INFO field
        info_dict = {key.split('=')[0]: key.split('=', 1)[1] for key in info.split(';') if '=' in key}

        # Extract specified INFO fields
        extracted_info = {field: info_dict.get(field, None) for field in info_fields}

        # Parse CSQ field
        csq_data = info_dict.get('CSQ', '').split(',')

        # Expand CSQ entries
        for csq_entry in csq_data:
            csq_fields = csq_entry.split('|')
            csq_dict = dict(zip(vep_headers, csq_fields))

            # Add fixed fields to the row
            csq_dict['CHROM'] = chrom
            csq_dict['POS'] = pos
            csq_dict['ID'] = id
            csq_dict['REF'] = ref
            csq_dict['ALT'] = alt
            csq_dict['QUAL'] = qual
            csq_dict['FILTER'] = filt
            csq_dict['FORMAT'] = fmt

            # Add INFO fields and sample data
            csq_dict.update(extracted_info)
            for sample, data in zip(sample_columns, sample_data):
                csq_dict[sample] = data

            parsed_rows.append(csq_dict)

    # Convert to DataFrame
    df = pd.DataFrame(parsed_rows)

    # Reorder columns: Fixed fields, sample columns, INFO fields, then the rest
    fixed_columns = ['CHROM', 'POS', 'ID', 'REF', 'ALT', 'QUAL', 'FILTER', 'FORMAT']
    all_columns = fixed_columns + sample_columns + info_fields + [
        col for col in df.columns if col not in fixed_columns + sample_columns + info_fields
    ]
    df = df[all_columns]

    # Drop empty columns (all NaN or empty string)
    df.replace('', pd.NA, inplace=True)
    df.dropna(axis=1, how='all', inplace=True)

    # Write to TSV
    df.to_csv(output_file, sep='\t', index=False)

if __name__ == '__main__':
    parser = argparse.ArgumentParser(description="Parse a VCF file (gzipped or plain) and extract INFO fields")
    parser.add_argument("input_vcf", help="Input VCF file (.vcf or .vcf.gz)")
    parser.add_argument("output_file", help="Output TSV file")
    args = parser.parse_args()

    parse_vcf(args.input_vcf, args.output_file)