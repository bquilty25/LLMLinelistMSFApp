#!/usr/bin/env python3
"""Wrapper for mlx_vlm generation with thinking disabled.

Usage:
    python3 scripts/mlx_generate.py --model MODEL --prompt PROMPT [--max-tokens N] [--temperature T] [--system SYSTEM]

Wraps mlx_vlm to properly disable Qwen3.5's thinking mode via enable_thinking=False,
which isn't exposed through the mlx_vlm CLI.
"""

import argparse
import sys
import time

from mlx_vlm import load, generate
from mlx_vlm.prompt_utils import apply_chat_template


def main():
    parser = argparse.ArgumentParser(description="Generate text with mlx_vlm (thinking disabled)")
    parser.add_argument("--model", required=True, help="HF model path")
    parser.add_argument("--prompt", required=True, help="User prompt (or path to file with @/path)")
    parser.add_argument("--system", default=None, help="System prompt")
    parser.add_argument("--max-tokens", type=int, default=16384, help="Max tokens to generate")
    parser.add_argument("--temperature", type=float, default=0.1, help="Sampling temperature")
    parser.add_argument("--repetition-penalty", type=float, default=1.1, help="Repetition penalty (1.1 is good default)")
    parser.add_argument("--repetition-context-size", type=int, default=20, help="Tokens to consider for repetition penalty")
    parser.add_argument("--max-kv-size", type=int, default=32768, help="Max KV cache size (default 32768 to avoid truncation)")
    parser.add_argument("--top-p", type=float, default=1.0, help="Top-p sampling")
    parser.add_argument("--kv-bits", type=float, default=None, help="KV cache quantization bits (e.g. 3.5 for TurboQuant)")
    parser.add_argument("--kv-quant-scheme", type=str, default=None, choices=("uniform", "turboquant"), help="KV cache quantization scheme")
    parser.add_argument("--verbose", action="store_true", help="Print verbose output")
    args = parser.parse_args()

    # Prepare prompt
    if args.prompt.startswith("@"):
        with open(args.prompt[1:], "r") as f:
            prompt_content = f.read()
    else:
        prompt_content = args.prompt

    # Load model and processor
    if args.verbose:
        print(f"Loading model: {args.model}", file=sys.stderr)
    model, processor = load(args.model, trust_remote_code=True)

    # Build messages
    messages = []
    if args.system:
        messages.append({"role": "system", "content": args.system})
    messages.append({"role": "user", "content": prompt_content})

    # Apply chat template with thinking DISABLED
    if args.verbose:
        print("Applying chat template (thinking disabled)...", file=sys.stderr)

    try:
        prompt = processor.apply_chat_template(
            messages,
            tokenize=False,
            add_generation_prompt=True,
            enable_thinking=False,
        )
    except TypeError:
        # Fallback for models whose chat template does not accept enable_thinking
        if args.verbose:
            print("Note: enable_thinking not supported for this model; using standard template.", file=sys.stderr)
        prompt = processor.apply_chat_template(
            messages,
            tokenize=False,
            add_generation_prompt=True,
        )

    # Count prompt tokens for fallback (GenerationResult object has this built-in)
    try:
        prompt_tokens_count = len(processor.tokenizer.encode(prompt))
    except Exception:
        prompt_tokens_count = -1

    if args.verbose:
        print(f"Prompt tokens (approx): {prompt_tokens_count}", file=sys.stderr)
        print("Generating...", file=sys.stderr)

    generate_args = {
        "max_tokens": args.max_tokens,
        "temp": args.temperature,
        "repetition_penalty": args.repetition_penalty,
        "repetition_context_size": args.repetition_context_size,
        "max_kv_size": args.max_kv_size,
        "top_p": args.top_p,
        "verbose": True,  # Stream tokens; we redirect stdout→stderr so log shows progress
    }

    if args.kv_bits is not None:
        generate_args["kv_bits"] = args.kv_bits
    if args.kv_quant_scheme is not None:
        generate_args["kv_quant_scheme"] = args.kv_quant_scheme

    # Redirect stdout -> stderr during generation so streamed tokens appear in the
    # run log in real-time, while stdout stays clean for the final response text.
    _old_stdout = sys.stdout
    sys.stdout = sys.stderr
    gen_start = time.perf_counter()
    result = generate(model, processor, prompt, **generate_args)
    gen_elapsed = time.perf_counter() - gen_start
    sys.stdout = _old_stdout
    print(file=sys.stderr)  # newline after streamed tokens before MLX_TIMING

    # GenerationResult object has built-in timing fields when verbose=False
    if hasattr(result, "text"):
        output_text = result.text
        prompt_tokens    = getattr(result, "prompt_tokens", -1)
        output_tokens    = getattr(result, "generation_tokens", -1)
        prompt_tps       = getattr(result, "prompt_tps", -1.0)
        generation_tps   = getattr(result, "generation_tps", -1.0)
        peak_memory_gb   = getattr(result, "peak_memory", None)
    else:
        # Fallback: plain string (older mlx_vlm versions)
        output_text = str(result)
        prompt_tokens = prompt_tokens_count  # from earlier tokenizer call
        output_tokens = -1
        prompt_tps    = -1.0
        generation_tps = gen_elapsed and output_tokens / gen_elapsed or -1.0
        peak_memory_gb = None

    print(
        f"MLX_TIMING: generation_secs={gen_elapsed:.3f} "
        f"prompt_tokens={prompt_tokens} "
        f"output_tokens={output_tokens} "
        f"prompt_tps={prompt_tps:.2f} "
        f"generation_tps={generation_tps:.2f}"
        + (f" peak_memory_gb={peak_memory_gb:.3f}" if peak_memory_gb is not None else ""),
        file=sys.stderr,
    )

    print(output_text)


if __name__ == "__main__":
    main()
