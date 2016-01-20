value := 350


.PHONY: brightness
brightness:
	@echo $(value) > /sys/class/backlight/intel_backlight/brightness

.PHONY: prune-packages
prune-packages:
	paccache -r && paccache -ruk0
