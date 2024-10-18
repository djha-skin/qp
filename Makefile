.PHONY: all clean

all: qp

qp: com.djhaskin.qp.asd src/*.lisp qp.ros .bundle-libs/bundle.lisp
	- ros build qp.ros

.bundle-libs/bundle.lisp: qlfile.lock
	- qlot bundle

qlfile.lock: qlfile
	- qlot install

qlfile:
	- qlot init
	- qlot install djha-skin/nrdl
	- qlot install djha-skin/cliff

clean:
	- rm -f qp