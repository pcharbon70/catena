# Catena proof workbench

This workbench independently encodes a deliberately small typed core and proves
substitution, context compatibility, preservation, progress, a component
interaction lemma, and their bounded composition. It is a kernel-checked start
to the G139 proof ledger. It is not the integrated Catena progress and
preservation theorem.

`rocq.lock.json` pins the checker and container digest used for the retained
run. `check.sh` copies the read-only source into the container's temporary
directory, so generated `.vo` and `.glob` files never become repository inputs.

Run `./proof/check.sh` from any directory with Docker available.
