import click
from .encode import encode_message
from .decode import decode_message
from . import utils


@click.group()
def steganography_tool() -> None:
    pass


@steganography_tool.command()
@click.argument('output_filename', type=click.Path())
@click.argument('secret_message', type=str)
def encode(output_filename: str, secret_message: str) -> None:
    en_message = encode_message(utils.get_base_file(), secret_message)
    utils.write_file(en_message, output_filename)


@steganography_tool.command()
@click.argument('input_filename', type=click.Path())
def decode(input_filename: str) -> None:
    message = decode_message(utils.read_file(input_filename))
    click.echo(message)


if __name__ == '__main__':
    steganography_tool()
