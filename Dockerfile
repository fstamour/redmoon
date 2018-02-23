FROM debian

RUN apt-get update
RUN apt-get install -y sbcl curl gnupg2

# Setup quicklisp
RUN curl -sOL https://beta.quicklisp.org/quicklisp.lisp
# RUN curl -sOL https://beta.quicklisp.org/quicklisp.lisp.asc
# RUN gpg --verify quicklisp.lisp.asc quicklisp.lisp
RUN sbcl --noinform --non-interactive --load quicklisp.lisp --eval '(quicklisp-quickstart:install)'
COPY docker/sbclrc /root/.sbclrc

WORKDIR /root/quicklisp/local-projects/redmoon
COPY . /root/quicklisp/local-projects/redmoon

RUN sbcl --noinform --eval '(ql:quickload :redmoon.test)' \
	--eval '(sb-ext:save-lisp-and-die "redmoon.core" :compression t)'

ENTRYPOINT ["sbcl", "--noinform", "--core", "redmoon.core"]
CMD ["--non-interactive", "--eval", "(asdf:test-system :redmoon)"]

