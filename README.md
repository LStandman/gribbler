# Paranoia
Someday this will be an ncurses-based password manager.

Currently what is here is a Haskell implementation of some popular cryptographic functions (AES256, SHA256, PBKDF2-HMAC), 
and a testing suite which uses some well known test-vectors.

# Why should anybody trust this project?
They shouldn't. 

However, this project does have some pros:
* FOSS - four freedoms, yadda yadda.
* Small code base - small attack surface.
* Minimal dependencies - a short chain of trust (more so for me :]).
* Functional programming - might be more of a taste thing, but the lack of side-effects *should* help in reducing unexpected behavior.
