all: all-src all-lib

clean: clean-src clean-lib

install: install-src install-lib

uninstall: uninstall-src uninstall-lib

reinstall: reinstall-src reinstall-lib

all-src:
	$(MAKE) all -C src 

all-lib:
	$(MAKE) all -C lib 

clean-src:
	$(MAKE) clean -C src 

clean-lib:
	$(MAKE) clean -C lib 

install-src:
	$(MAKE) install -C src 

install-lib:
	$(MAKE) install -C lib 

uninstall-src:
	$(MAKE) uninstall -C src 

uninstall-lib:
	$(MAKE) uninstall -C lib 

reinstall-src:
	$(MAKE) reinstall -C src 

reinstall-lib:
	$(MAKE) reinstall -C lib 
