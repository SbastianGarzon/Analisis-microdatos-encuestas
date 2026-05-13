import pandas as pd
import numpy as np
import json
import warnings
from collections import defaultdict

warnings.filterwarnings('ignore')

def parse_json_column(value):
    """Parse JSON-like string values safely"""
    if pd.isna(value) or value == '':
        return None
    if isinstance(value, str):
        try:
            # Try to parse as JSON
            return json.loads(value)
        except:
            # If not JSON, return as is
            return value
    return value

def calculate_weighted_percentage(df, column, weight_col='weight'):
    """Calculate weighted percentage for a given column"""
    # Remove rows with missing values in the column or weight
    valid_mask = df[column].notna() & df[weight_col].notna()
    df_valid = df[valid_mask].copy()
    
    if len(df_valid) == 0:
        return {}
    
    # Group by unique values and sum weights
    grouped = df_valid.groupby(column)[weight_col].sum()
    total_weight = grouped.sum()
    
    # Calculate percentages
    if total_weight > 0:
        percentages = (grouped / total_weight).to_dict()
    else:
        percentages = grouped.to_dict()
    
    return percentages

def process_json_column(df, column, weight_col='weight'):
    """Process columns containing JSON arrays of label-value pairs"""
    results = defaultdict(lambda: defaultdict(float))
    total_weights = defaultdict(float)
    
    for idx, row in df.iterrows():
        if pd.isna(row[column]) or pd.isna(row[weight_col]):
            continue
            
        weight = row[weight_col]
        json_data = parse_json_column(row[column])
        
        if json_data and isinstance(json_data, list):
            for item in json_data:
                if isinstance(item, dict) and 'label' in item and 'value' in item:
                    label = item['label']
                    value = item['value']
                    results[label][value] += weight
                    total_weights[label] += weight
    
    # Convert to percentages
    percentages = {}
    for label, values in results.items():
        if total_weights[label] > 0:
            percentages[label] = {
                value: count / total_weights[label]
                for value, count in values.items()
            }
        else:
            percentages[label] = values
            
    return percentages

def main():
    # Read the CSV file
    print("Reading CSV file...")
    df = pd.read_csv('Atlas_Semana_E226_Raw_Data_020526.csv')
    
    print(f"Total rows: {len(df)}")
    print(f"Total columns: {len(df.columns)}")
    
    # Identify columns to skip
    skip_columns = ['user_id', 'weight']
    
    # Dictionary to store all results
    all_results = {}
    
    # Process each column
    for column in df.columns:
        if column in skip_columns:
            continue
            
        print(f"\nProcessing column: {column}")
        
        # Check if column contains JSON data
        sample_values = df[column].dropna().head(5)
        is_json_column = False
        
        for value in sample_values:
            if isinstance(value, str) and value.startswith('[') and value.endswith(']'):
                is_json_column = True
                break
        
        if is_json_column:
            # Process as JSON column
            print(f"  - Processing as JSON column")
            result = process_json_column(df, column)
            if result:
                all_results[column] = result
        else:
            # Process as regular column
            print(f"  - Processing as regular column")
            result = calculate_weighted_percentage(df, column)
            if result:
                all_results[column] = result
                
                # Verify that percentages sum to 1 (or very close)
                total = sum(result.values())
                print(f"  - Total percentage: {total:.6f}")
    
    # Add metadata
    metadata = {
        "_metadata": {
            "total_rows": len(df),
            "total_respondents": df['user_id'].nunique(),
            "total_weight": df['weight'].sum(),
            "processed_columns": len(all_results)
        }
    }
    
    # Combine results with metadata
    final_output = {**metadata, **all_results}
    
    # Save to JSON file
    output_path = 'weighted_averages_results.json'
    with open(output_path, 'w', encoding='utf-8') as f:
        json.dump(final_output, f, ensure_ascii=False, indent=2)
    
    print(f"\nResults saved to: {output_path}")
    
    # Also create a summary report
    summary_path = 'Atlas Semana E126 Raw Data 010926.csv'
    with open(summary_path, 'w', encoding='utf-8') as f:
        f.write("WEIGHTED AVERAGES SUMMARY REPORT\n")
        f.write("================================\n\n")
        f.write(f"Total respondents: {metadata['_metadata']['total_respondents']}\n")
        f.write(f"Total weight sum: {metadata['_metadata']['total_weight']:.6f}\n")
        f.write(f"Processed columns: {metadata['_metadata']['processed_columns']}\n\n")
        
        for column, results in all_results.items():
            f.write(f"\n{column}:\n")
            f.write("-" * (len(column) + 1) + "\n")
            
            if isinstance(results, dict) and all(isinstance(v, dict) for v in results.values()):
                # JSON column with nested structure
                for label, values in results.items():
                    f.write(f"\n  {label}:\n")
                    sorted_values = sorted(values.items(), key=lambda x: x[1], reverse=True)
                    for value, percentage in sorted_values:
                        f.write(f"    {value}: {percentage:.4%}\n")
            else:
                # Regular column
                sorted_results = sorted(results.items(), key=lambda x: x[1], reverse=True)
                for value, percentage in sorted_results:
                    f.write(f"  {value}: {percentage:.4%}\n")
    
    print(f"Summary report saved to: {summary_path}")
    
    # Display a sample of results
    print("\n\nSAMPLE RESULTS:")
    print("===============")
    
    # Show first few regular columns
    regular_count = 0
    for column, results in all_results.items():
        if regular_count >= 3:
            break
        if not isinstance(results, dict) or not all(isinstance(v, dict) for v in results.values()):
            print(f"\n{column}:")
            sorted_results = sorted(results.items(), key=lambda x: x[1], reverse=True)[:5]
            for value, percentage in sorted_results:
                print(f"  {value}: {percentage:.4%}")
            regular_count += 1
    
    # Show JSON column example if exists
    for column, results in all_results.items():
        if isinstance(results, dict) and all(isinstance(v, dict) for v in results.values()):
            print(f"\n{column} (JSON column - first label only):")
            first_label = list(results.keys())[0]
            print(f"  Label: {first_label}")
            sorted_values = sorted(results[first_label].items(), key=lambda x: x[1], reverse=True)
            for value, percentage in sorted_values:
                print(f"    {value}: {percentage:.4%}")
            break

if __name__ == "__main__":
    main()
