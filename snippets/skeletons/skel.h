#ifndef ${1:$(upcase yas/text)}_H
#define ${1:$(upcase yas/text)}_H

/*
  ${1:$$(file-name-nondirectory (file-name-sans-extension (buffer-file-name)))}.h

  Created on `(format-time-string "%B %e, %Y")`
*/

class ${1:$(capitalize yas/text)} {
  public:
	${1:$(capitalize yas/text)}();
	~${1:$(capitalize yas/text)}();

  private:
    $0
};

#endif /* ${1:$(upcase yas/text)} */
