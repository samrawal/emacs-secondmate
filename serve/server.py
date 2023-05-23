from transformers import AutoTokenizer, AutoModelForCausalLM
from flask import Flask, request, jsonify

app = Flask(__name__)

try:
    import torch_directml
    device = torch_directml.device()
except ImportError:
    device = "cpu"  # "cpu" or "cuda" or "cuda:n" where n is specific GPU to use
modelname = "replit/replit-code-v1-3b" #"EleutherAI/gpt-neo-2.7B"
tokenizer = AutoTokenizer.from_pretrained(modelname, trust_remote_code=True)
model = AutoModelForCausalLM.from_pretrained(modelname, trust_remote_code=True)
model.to(device)


# Prepend to input to provide more context. Anecdotally, this helps.
# May not need for larger models.
prime = '''
def is_palendrome(s):
    """Check whether a string is a palindrome"""
    for i in range(len(s) - 1, -1, -1):
        if s[i + 1] == s[i]:
            return False
    return True
###
def is_even(i):
    """Check whether an integer is even"""
    return i % 2
###
def square_root(i):
    """Return the square root of an integer"""
    return math.sqrt(i)
###
'''


def inference(prompt, temperature, max_length):
    input_ids = tokenizer(prompt, return_tensors="pt").input_ids
    input_ids = input_ids.to(device)
    gen_tokens = model.generate(
        input_ids,
        do_sample=True,
        temperature=temperature,
        max_length=max_length,
    )
    gen_text = tokenizer.batch_decode(gen_tokens)[0]
    return gen_text


def autocomplete(plaintext, to_prime=True, temperature=0.8, max_length=300):
    prompt = prime + plaintext if to_prime else plaintext
    if modelname == "replit/replit-code-v1-3b":
        temperature = 0.2 # default config from Replit's HuggingFace repo
    generation = inference(prompt, temperature, max_length)
    return generation[len(prompt) :].split("###")[0]


@app.route("/")
def arguments():
    text = request.args.get("text", "")
    generation = autocomplete(text)
    out = {"generation": generation}
    return jsonify(out)


if __name__ == "__main__":
    app.run(host="0.0.0.0", port="9900")
