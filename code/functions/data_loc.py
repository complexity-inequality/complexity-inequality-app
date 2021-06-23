#!/usr/bin/env python3

"""
Python function to get geospatial information about a Brazilian state
"""

import requests
import json
import pandas as pd
import janitor


def get_loc(ufs: list) -> pd.DataFrame:
  """
  This function gets spatial information from IBGE's API and return it as a pandas DataFrame.
  INPUT:
      ufs -> list (list with the ufs of interest)
  Output:
      df -> pd.DataFrame
  """

  if ufs == ["all"]:
    ufs = ["AC", "AL", "AP", "AM", "BA", "CE", "ES", "GO", "MA", "MT", "MS", "MG", "PA", "PB", "PR", "PE", "PI", "RJ", "RN", "RS", "RO", "RR", "SC", "SP", "SE", "TO", "DF"]
  
  ufsj = "|".join(ufs)
  
  try:
    response = requests.get(f"https://servicodados.ibge.gov.br/api/v1/localidades/estados/{ufsj}/municipios")
  except requests.exceptions.HTTPError as errh:
    print(errh)
  except requests.exceptions.ConnectionError as errc:
    print(errc)
  except requests.exceptions.Timeout as errt:
    print(errt)
  except requests.exceptions.RequestException as err:
    print(err)
  
  df = pd.json_normalize(json.loads(response.text)).clean_names()
  
  return df
