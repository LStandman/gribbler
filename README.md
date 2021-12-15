# Gribbler
Someday this will be an ncurses-based password manager.

Already includes in-house implementations of some popular cryptographic functions (AES256, SHA256, PBKDF2-HMAC) 
and a testing suite that uses some common test-vectors.

# Should anybody trust this project?
No. 

However, this project does have some pros:
* Small code base - small attack surface.
* Minimal dependencies - short chain of trust (more so for me :]).
* Functional programming - the lack of side-effects *should* help to reduce unexpected behavior.
