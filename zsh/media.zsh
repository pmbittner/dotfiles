# Images, PDFs and documents
# Converting and composing images, PDFs and QR codes.
# Sourced by ~/.zshrc.

## other utilities
pb-shrink-all-pngs () {
  mkdir small
  find -maxdepth 1 -name "*.png" -exec convert {} -resize 2048x2048 small/{} \;
}
pb-shrink-all-jpgs () {
  mkdir small
  find -maxdepth 1 -regextype sed -regex ".*.jpe\?g" -exec convert {} -resize 2048x2048 small/{} \;
}

pb-convert-pngs-to-jpgs () {
  find -maxdepth 1 -name "*.png" -exec convert {} {}.jpg \;
}

## argument is a file ending like "png" or "jpg"
## Then, all such files in the current directory will be composed into a single big pdf.
## Each image file take exactly one pdf page.
## Images are ordered alphabetically.
pb-compose-to-pdf () {
  convert "*.$@" -auto-orient composed.pdf
}
pb-compose-pngs-to-pdf () {
  pb-compose-to-pdf png
}
pb-compose-jpgs-to-pdf () {
  pb-compose-to-pdf jpg
}

pb-scans-to-pdf () {
  pb-shrink-all-pngs
  cd small
  pb-convert-pngs-to-jpgs
  pb-compose-jpgs-to-pdf
  mv composed.pdf ..
  cd ..
  rm -rf small
}

pb-generate-qr-code () {
  usage () {
    echo "This script takes the following arguments:"
    echo "1. (mandatory): name of the qr code file (without file ending)"
    echo "2. (mandatory): url for the QR code"
    echo "3. (optional) : latex options for the fancyqr package (e.g., image=\huge\faGithub). See https://ctan.org/pkg/fancyqr."
  }
  ORIGINAL_DIR="$(pwd)"

  if [[ -z "$1" ]]; then
    print -u2 "ERROR: File name expected as first argument."
    usage
    return 1
  fi

  if [[ -z "$2" ]]; then
    print -u2 "ERROR: URL expected as second argument."
    usage
    return 1
  fi

  if [ -n "$3" ]; then
    QR_OPTIONS="$3"
  else
    QR_OPTIONS=""
  fi

  FANCY_QR_TEX="\documentclass{article}
\usepackage{fontawesome}
\usepackage{qrcode}
\usepackage{fancyqr}
\usepackage[active,tightpage]{preview}
\FancyQrLoad{flat}
\fancyqrset{padding,gradient=false,color=black}
\begin{document}
  \begin{preview}
    \fancyqr[${QR_OPTIONS}]{$2}
  \end{preview}
\end{document}
"

  PLAIN_QR_TEX="\documentclass{standalone}
\usepackage{fontawesome}
\usepackage{qrcode}
\usepackage{hyperref}
\begin{document}
  \href{$2}{\qrcode{$2}}
\end{document}
"

  # QR_TEX=${PLAIN_QR_TEX}
  QR_TEX=${FANCY_QR_TEX}

  QR_DIR="${HOME}/usrtemp/generate-qr-code"
  QR_NAME="$1"
  QR_TEX_FILE="${QR_NAME}.tex"
  QR_PDF_FILE="${QR_NAME}.pdf"

  mkdir -p ${QR_DIR}
  cd ${QR_DIR}

  # use printf instead of echo to not interpret backslashes
  printf '%s' "${QR_TEX}" > ${QR_TEX_FILE}
  latexmk -quiet -silent -pdf -interaction=nonstopmode ${QR_TEX_FILE}
  # latexmk -interaction=nonstopmode ${QR_TEX_FILE} # use this line to debug
  cp "${QR_PDF_FILE}" "${ORIGINAL_DIR}"
  # ev ${QR_PDF_FILE}
  latexmk -C ${QR_TEX_FILE}

  cd ${ORIGINAL_DIR}
  rm -r ${QR_DIR}
}

pb-compress-pdf () {
  OUTPUT_FILE="$1-compressed.pdf"
  gs \
    -sDEVICE=pdfwrite \
    -dPDFSETTINGS=/default \
    -dNOPAUSE \
    -dQUIET \
    -dBATCH \
    -sOutputFile="${OUTPUT_FILE}" \
    "$1"
  echo "Wrote file ${OUTPUT_FILE} (if the previous command succeeded)."
}

pb-md-to-org () {
  pandoc "$@.md" -o "$@.org"
}
