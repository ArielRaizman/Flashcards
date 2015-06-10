PROJECT = flashcards

DEPS = cowboy erlydtl

ERLC_OPTS = +debug_info -Ddebug

SHELL_DEPS = sync
SHELL_OPTS = -s sync go -s flashcards_app -flashcards http_port 8080

include erlang.mk
