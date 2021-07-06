import requests, sys

url = "localhost:9900"
params = {"text": sys.argv[1:]}
generation = requests.get(url, params).json()["generation"]
print(generation)
