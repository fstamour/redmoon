FROM debian

RUN apt-get update
RUN apt-get install -y sbcl curl gnupg2

# Setup quicklisp
RUN curl -sOL https://beta.quicklisp.org/quicklisp.lisp
# RUN curl -sOL https://beta.quicklisp.org/quicklisp.lisp.asc
# RUN gpg --verify quicklisp.lisp.asc quicklisp.lisp
RUN sbcl --noinform --non-interactive --load quicklisp.lisp --eval '(quicklisp-quickstart:install)'
COPY ci/sbclrc /root/.sbclrc

# Fetching the dependencies first
RUN sbcl --noinform --eval "(map nil 'ql:quickload '(anaphora alexandria parachute))"

WORKDIR /root/quicklisp/local-projects/redmoon
COPY . /root/quicklisp/local-projects/redmoon

# Load the project and dump a core (for quicker loads)
RUN sbcl --noinform --eval '(ql:quickload :redmoon.test)' \
	--eval '(sb-ext:save-lisp-and-die "redmoon.core" :compression t)'

ENTRYPOINT ["sbcl", "--noinform", "--core", "redmoon.core"]
CMD ["--non-interactive", "--eval", "(asdf:test-system :redmoon)"]
