PROJECT = flashcards

DEPS = cowboy erlydtl

ERLC_OPTS = +debug_info -Ddebug

SHELL_OPTS = -s flashcards_app -flashcards http_port 8080

include erlang.mk
