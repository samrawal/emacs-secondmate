from transformers import AutoTokenizer, AutoModelForCausalLM
from flask import Flask, request, jsonify


DEVICE = "cpu"
MODELNAME = "replit/replit-code-v1-3b" # "Salesforce/codegen-16B-mono", "EleutherAI/gpt-neo-2.7B"


# Prepend to input to provide more context. Anecdotally, this helps.
# May not need for larger models.
PRIME = '''
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


app = Flask(__name__)


def inference(prompt, temperature, max_length):
    if "Salesforce/codegen" in MODELNAME:
        inputs = TOKENIZER(prompt, return_tensors="pt").to(DEVICE)
        sample = MODEL.generate(**inputs, max_length=max_length)
        gen_text = "\n".join(
            [
                TOKENIZER.decode(sample[0]),
                "###",
            ]
        )
    else:
        input_ids = TOKENIZER(prompt, return_tensors="pt").input_ids
        input_ids = input_ids.to(DEVICE)
        gen_tokens = MODEL.generate(
            input_ids,
            do_sample=True,
            temperature=temperature,
            max_length=max_length,
        )
        gen_text = TOKENIZER.batch_decode(gen_tokens)[0]
    print(repr(gen_text))
    return gen_text


def autocomplete(plaintext, to_prime=None, temperature=0.8, max_length=300):
    if to_prime is None:
        to_prime = all(x not in MODELNAME for x in ("Salesforce", "replit"))
    if MODELNAME == "replit/replit-code-v1-3b":
        temperature = 0.2 # default config from Replit's HuggingFace repo
    prompt = PRIME + plaintext if to_prime else plaintext
    generation = inference(prompt, temperature, max_length)
    return generation[len(prompt) :].split("###")[0]


@app.route("/")
def arguments():
    text = request.args.get("text", "")
    generation = autocomplete(text)
    out = {"generation": generation}
    return jsonify(out)


def main(arguments=None):
    global DEVICE, MODELNAME, TOKENIZER, MODEL
    if arguments:
        if arguments.device == "directml":
            import torch_directml  # pyright: ignore

            DEVICE = torch_directml.device()
        else:
            DEVICE = arguments.device
        if arguments.model:
            MODELNAME = arguments.model
    TOKENIZER = AutoTokenizer.from_pretrained(MODELNAME, trust_remote_code=True)
    MODEL = AutoModelForCausalLM.from_pretrained(MODELNAME, trust_remote_code=True)
    MODEL.to(DEVICE)
    app.run(host="0.0.0.0", port=9900)


if __name__ == "__main__":
    import sys
    from argparse import ArgumentParser

    parser = ArgumentParser()
    parser.add_argument(
        "--model",
        help="""Base model to use. We have support code currently for:
        EleutherAI/gpt-neo-2.7B
        replit/replit-code-v1-3b
        Salesforce/codegen-{350M,2B,6B,16B}-{nl,multi,mono}
        """,
        default=MODELNAME,
        required=False,
    )
    parser.add_argument(
        "--device",
        help="cpu|cuda|directml|cuda:{n} (where n is the GPU to use)",
        default=DEVICE,
        required=False,
    )
    args = parser.parse_args(sys.argv[1:])
    main(args)
