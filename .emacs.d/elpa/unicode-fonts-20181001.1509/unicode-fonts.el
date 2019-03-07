;;; unicode-fonts.el --- Configure Unicode fonts
;;
;; Copyright (c) 2012-2015 Roland Walker
;;
;; Author: Roland Walker <walker@pobox.com>
;; Homepage: http://github.com/rolandwalker/unicode-fonts
;; URL: http://raw.githubusercontent.com/rolandwalker/unicode-fonts/master/unicode-fonts.el
;; Package-Version: 20181001.1509
;; Version: 0.4.10
;; Last-Updated:  1 Oct 2018
;; EmacsWiki: UnicodeFonts
;; Keywords: i18n, faces, frames, wp, interface
;; Package-Requires: ((font-utils "0.7.8") (ucs-utils "0.8.2") (list-utils "0.4.2") (persistent-soft "0.8.10") (pcache "0.3.1"))
;;
;; Simplified BSD License
;;
;;; Commentary:
;;
;; Quickstart:
;;
;;     Configure an extended Latin font for your default face, such
;;     as Monaco, Consolas, or DejaVu Sans Mono.
;;
;;     Install these fonts
;;
;;         http://users.teilar.gr/~g1951d/Symbola.zip
;;         http://www.quivira-font.com/files/Quivira.ttf   ; or Quivira.otf
;;         http://sourceforge.net/projects/dejavu/files/dejavu/2.37/dejavu-fonts-ttf-2.37.tar.bz2
;;         https://github.com/googlei18n/noto-fonts/raw/master/hinted/NotoSans-Regular.ttf
;;         https://github.com/googlei18n/noto-fonts/raw/master/unhinted/NotoSansSymbols-Regular.ttf
;;
;;     Remove Unifont from your system.
;;
;;     (require 'unicode-fonts)
;;
;;     (unicode-fonts-setup)
;;
;; Testing:
;;
;;     C-h h                                         ; M-x view-hello-file
;;     M-x list-charset-chars RET unicode-bmp RET    ; search for 210x
;;     M-x list-charset-chars RET unicode-smp RET    ; if your backend supports astral chars
;;     M-x unicode-fonts-debug-insert-block RET Mathematical_Operators RET
;;
;; Explanation:
;;
;; Emacs maintains font mappings on a per-glyph basis, meaning
;; that multiple fonts are used at the same time (transparently) to
;; display any character for which you have a font.  Furthermore,
;; Emacs does this out of the box.
;;
;; However, font mappings via fontsets are a bit difficult to
;; configure.  In addition, the default setup does not always pick
;; the most legible fonts.  As the manual warns, the choice of font
;; actually displayed for a non-ASCII character is "somewhat random".
;;
;; The Unicode standard provides a way to organize font mappings: it
;; divides character ranges into logical groups called "blocks".  This
;; library configures Emacs in a Unicode-friendly way by providing
;; mappings from
;;
;;     each Unicode block  ---to--->   a font with good coverage
;;
;; and makes the settings available via the customization interface.
;;
;; This library provides font mappings for 233 of the 255 blocks in
;; the Unicode 8.0 standard which are public and have displayable
;; characters.  It assumes that 6 Latin blocks are covered by the
;; default font.  16/255 blocks are not mapped to any known font.
;;
;; To use unicode-fonts, place the unicode-fonts.el file somewhere
;; Emacs can find it, and add the following to your ~/.emacs file:
;;
;;     (require 'unicode-fonts)
;;     (unicode-fonts-setup)
;;
;; See important notes about startup speed below.
;;
;; To gain any benefit from the library, you must have fonts with good
;; Unicode support installed on your system.  If you are running a
;; recent version of OS X or Microsoft Windows, you already own some
;; good multi-lingual fonts, though you would do very well to download
;; and install the four items below:
;;
;; From https://dejavu-fonts.github.io/
;;
;;     DejaVu Sans, DejaVu Sans Mono
;;
;; From http://www.quivira-font.com/downloads.php
;;
;;     Quivira
;;
;; From http://users.teilar.gr/~g1951d/Symbola.zip
;;
;;     Symbola
;;
;; Many non-free fonts are referenced by the default settings.
;; However, free alternatives are also given wherever possible, and
;; patches are of course accepted to improve every case.
;;
;; On the assumption that an extended Latin font such as Monaco,
;; Consolas, or DejaVu Sans Mono is already being used for the default
;; face, no separate mappings are provided for the following Unicode
;; blocks:
;;
;;     Basic Latin
;;     Latin Extended Additional
;;     Latin Extended-A
;;     Latin Extended-B
;;     Latin-1 Supplement
;;     Spacing Modifier Letters
;;
;; though some of these remain configurable via `customize'.
;;
;; It is also recommended to remove GNU Unifont from your system.
;; Unifont is very useful for debugging, but not useful for reading.
;;
;; The default options favor correctness and completeness over speed,
;; and can add many seconds to initial startup time in GUI mode.
;; However, when possible a font cache is kept between sessions.  If
;; you have persistent-soft.el installed, when you start Emacs the
;; second time, the startup cost should be negligible.
;;
;; The disk cache will be rebuilt during Emacs startup whenever a font
;; is added or removed, or any relevant configuration variables are
;; changed.  To increase the speed of occasionally building the disk
;; cache, you may use the customization interface to remove fonts from
;; `unicode-fonts-block-font-mapping' which are not present on your
;; system.
;;
;; If you are using a language written in Chinese or Arabic script,
;; try customizing `unicode-fonts-skip-font-groups' to control which
;; script you see, and send a friendly bug report.
;;
;; Color Emoji are enabled by default when using the Native Mac port
;; on OS X.  This can be disabled by customizing each relevant mapping,
;; or by turning off all multicolor glyphs here:
;;
;;     M-x customize-variable RET unicode-fonts-skip-font-groups RET
;;
;; See Also
;;
;;     M-x customize-group RET unicode-fonts RET
;;     M-x customize-variable RET unicode-fonts-block-font-mapping RET
;;
;; Notes
;;
;; Free fonts recognized by this package may be downloaded from the
;; following locations.  For any language, it is increasingly likely
;; that Noto Sans provides coverage:
;;
;;     From http://www.google.com/get/noto/
;;
;;         Noto Sans and friends         ; 181 Unicode blocks and counting; sole
;;                                       ; source for these blocks:
;;                                       ;
;;                                       ;   Bamum / Bamum Supplement / Kaithi
;;                                       ;   Mandaic / Meetei Mayek Extensions
;;                                       ;   Sundanese Supplement
;;                                       ;
;;                                       ; Also a good source for recently-added
;;                                       ; glyphs such as "Turkish Lira Sign".
;;
;;     From http://scripts.sil.org/cms/scripts/page.php?item_id=CharisSIL_download
;;       or http://scripts.sil.org/cms/scripts/page.php?item_id=DoulosSIL_download
;;
;;         Charis SIL or Doulos SIL      ; Extended European and diacritics
;;
;;     From http://scripts.sil.org/cms/scripts/page.php?item_id=Gentium_download
;;
;;         Gentium Plus                  ; Greek
;;
;;     From http://users.teilar.gr/~g1951d/
;;
;;         Aegean, Aegyptus, Akkadian    ; Ancient languages
;;         Analecta                      ; Ancient languages, Deseret
;;         Anatolian                     ; Ancient languages
;;         Musica                        ; Musical Symbols
;;         Nilus                         ; Ancient languages
;;
;;     From http://www.wazu.jp/gallery/views/View_MPH2BDamase.html
;;
;;         MPH 2B Damase                 ; Arabic, Armenian, Buginese, Cherokee, Georgian,
;;                                       ; Glagolitic, Hanunoo, Kharoshthi, Limbu, Osmanya,
;;                                       ; Shavian, Syloti Nagri, Tai Le, Thaana
;;
;;     From http://scripts.sil.org/cms/scripts/page.php?site_id=nrsi&id=NamdhinggoSIL
;;
;;         Namdhinggo SIL                ; Limbu
;;
;;     From http://wenq.org/wqy2/index.cgi?FontGuide
;;
;;         WenQuanYi Zen Hei             ; CJK (Simplified Chinese)
;;
;;     From http://babelstone.co.uk/Fonts/
;;
;;         BabelStone Han                ; CJK (Simplified Chinese)
;;         BabelStone Phags-pa Book      ; Phags-pa
;;         BabelStone Modern             ; Tags / Specials / Selectors
;;
;;     From http://vietunicode.sourceforge.net/fonts/fonts_hannom.html
;;
;;         HAN NOM A, HAN NOM B          ; CJK (Nôm Chinese)
;;
;;     From http://kldp.net/projects/unfonts/
;;
;;         Un Batang                     ; CJK (Hangul)
;;
;;     From http://sourceforge.jp/projects/hanazono-font/releases/
;;
;;         Hana Min A, Hana Min B        ; CJK (Japanese)
;;
;;     From http://scripts.sil.org/cms/scripts/page.php?site_id=nrsi&id=SILYi_home
;;
;;         Nuosu SIL                     ; CJK (Yi)
;;
;;     From http://www.daicing.com/manchu/index.php?page=fonts-downloads
;;
;;         Daicing Xiaokai               ; Mongolian
;;
;;     From http://www.library.gov.bt/IT/fonts.html
;;
;;         Jomolhari                     ; Tibetan
;;
;;     From http://www.thlib.org/tools/scripts/wiki/tibetan%20machine%20uni.html
;;
;;         Tibetan Machine Uni           ; Tibetan
;;
;;     From http://scripts.sil.org/cms/scripts/page.php?item_id=Padauk
;;
;;         Padauk                        ; Myanmar
;;
;;     From https://code.google.com/p/myanmar3source/downloads/list
;;
;;         Myanmar3                      ; Myanmar
;;
;;     From http://www.yunghkio.com/unicode/
;;
;;         Yunghkio                      ; Myanmar
;;
;;     From https://code.google.com/p/tharlon-font/downloads/list
;;
;;         TharLon                       ; Myanmar
;;
;;     From http://sourceforge.net/projects/prahita/files/Myanmar%20Unicode%20Fonts/MasterpieceUniSans/
;;
;;         Masterpiece Uni Sans          ; Myanmar
;;
;;     From http://sarovar.org/projects/samyak/
;;
;;         Samyak                        ; Gujarati, Malayalam, Oriya, Tamil
;;
;;     From http://software.sil.org/annapurna/download/
;;
;;         Annapurna SIL                 ; Devanagari
;;
;;     From http://guca.sourceforge.net/typography/fonts/anmoluni/
;;
;;         AnmolUni                      ; Gurmukhi
;;
;;     From http://brahmi.sourceforge.net/downloads2.html
;;
;;         Kedage                        ; Kannada
;;
;;     From http://www.omicronlab.com/bangla-fonts.html
;;
;;         Mukti Narrow                  ; Bengali
;;
;;     From http://www.kamban.com.au/downloads.html
;;
;;         Akshar Unicode                ; Sinhala
;;
;;     From http://tabish.freeshell.org/eeyek/download.html
;;
;;         Eeyek Unicode                 ; Meetei Mayek
;;
;;     From http://scripts.sil.org/CMS/scripts/page.php?&item_id=Mondulkiri
;;
;;         Khmer Mondulkiri              ; Khmer
;;
;;     From http://www.laoscript.net/downloads/
;;
;;         Saysettha MX                  ; Lao
;;
;;     From http://www.geocities.jp/simsheart_alif/taithamunicode.html
;;
;;         Lanna Alif                    ; Tai Tham
;;
;;     From http://scripts.sil.org/cms/scripts/page.php?site_id=nrsi&id=DaiBannaSIL
;;
;;         Dai Banna SIL                 ; New Tai Lue
;;
;;     From http://scripts.sil.org/cms/scripts/page.php?item_id=TaiHeritage
;;
;;         Tai Heritage Pro              ; Tai Viet
;;
;;     From http://sabilulungan.org/aksara/
;;
;;         Sundanese Unicode             ; Sundanese
;;
;;     From http://www.amirifont.org/
;;
;;         Amiri                         ; Arabic (Naskh)
;;
;;     From http://scripts.sil.org/cms/scripts/page.php?item_id=Scheherazade
;;
;;         Scheherazade                  ; Arabic (Naskh)
;;
;;     From http://www.farsiweb.ir/wiki/Persian_fonts
;;
;;         Koodak                        ; Arabic (Farsi)
;;
;;     From http://openfontlibrary.org/font/ahuramazda/
;;
;;         Ahuramzda                     ; Avestan
;;
;;     From http://scripts.sil.org/cms/scripts/page.php?site_id=nrsi&id=AbyssinicaSIL
;;
;;         Abyssinica SIL                ; Ethiopic
;;
;;     From http://www.bethmardutho.org/index.php/resources/fonts.html
;;
;;         Estrangelo Nisibin            ; Syriac
;;
;;     From http://www.evertype.com/fonts/nko/
;;
;;         Conakry                       ; N'ko
;;
;;     From http://uni.hilledu.com/download-ribenguni
;;
;;         Ribeng                        ; Chakma
;;
;;     From http://www.virtualvinodh.com/downloads
;;
;;         Adinatha Tamil Brahmi         ; Brahmi
;;
;;     From http://ftp.gnu.org/gnu/freefont/
;;
;;         FreeMono, etc (FreeFont)      ; Kayah Li (and others)
;;
;;     From http://ulikozok.com/aksara-batak/batak-font/
;;
;;         Batak-Unicode                 ; Batak
;;
;;     From http://scripts.sil.org/cms/scripts/page.php?site_id=nrsi&id=Mingzat
;;
;;         Mingzat                       ; Lepcha
;;
;;     From http://phjamr.github.io/lisu.html#install
;;          http://phjamr.github.io/miao.html#install
;;          http://phjamr.github.io/mro.html#install
;;
;;         Miao Unicode                  ; Miao
;;         Lisu Unicode                  ; Lisu
;;         Mro Unicode                   ; Mro
;;
;;     From http://scholarsfonts.net/cardofnt.html
;;
;;         Cardo                         ; Historical Languages
;;
;;     From http://sourceforge.net/projects/junicode/files/junicode/
;;
;;         Junicode                      ; Historical Languages
;;
;;     From http://www.evertype.com/fonts/vai/
;;
;;         Dukor                         ; Vai
;;
;;     From http://sourceforge.net/projects/zhmono/
;;
;;         ZH Mono                       ; Inscriptional Pahlavi / Parthian
;;
;;     From http://culmus.sourceforge.net/ancient/index.html
;;
;;         Aramaic Imperial Yeb          ; Imperial Aramaic
;;
;;     From http://www.languagegeek.com/font/fontdownload.html
;;
;;         Aboriginal Sans               ; Aboriginal Languages
;;         Aboriginal Serif
;;
;;     From http://scripts.sil.org/cms/scripts/page.php?site_id=nrsi&id=EzraSIL_Home
;;
;;         Ezra SIL                      ; Hebrew
;;
;;     From http://www.evertype.com/fonts/coptic/
;;
;;         Antinoou                      ; Coptic / General Punctuation
;;
;;     From http://apagreekkeys.org/NAUdownload.html
;;
;;         New Athena Unicode            ; Ancient Languages / Symbols
;;
;;     From http://markmail.org/thread/g57mk4sbdycblxds
;;
;;         KhojkiUnicodeOT               ; Khojki
;;
;;     From https://github.com/andjc/ahom-unicode/tree/master/font
;;
;;         AhomUnicode                   ; Ahom
;;
;;     From https://github.com/MihailJP/oldsindhi/releases
;;
;;         OldSindhi                     ; Khudawadi
;;
;;     From https://github.com/MihailJP/Muktamsiddham/releases
;;
;;         MuktamsiddhamG                ; Siddham  (note trailing "G" on font name)
;;
;;     From https://github.com/MihailJP/MarathiCursive/releases
;;
;;         MarathiCursiveG               ; Modi  (note trailing "G" on font name)
;;
;;     From https://github.com/OldHungarian/old-hungarian-font/releases
;;
;;         OldHungarian                  ; Old Hungarian
;;
;;     From http://tutohtml.perso.sfr.fr/unicode.html
;;
;;         Albanian                      ; Elbasan / Takri / Sharada
;;
;;     From https://github.com/enabling-languages/cham-unicode/tree/master/fonts/ttf
;;
;;         Cham OI_Tangin                ; Cham
;;
;;     From https://ctan.org/tex-archive/fonts/Asana-Math?lang=en
;;
;;         Asana Math                    ; Mathematical Symbols
;;
;; Compatibility and Requirements
;;
;;     GNU Emacs version 23.3 and higher : yes
;;     GNU Emacs version 22.3 and lower  : no
;;
;;     Requires font-utils.el, ucs-utils.el
;;
;; Bugs
;;
;;     The default choice of font for each code block balances coverage
;;     versus appearance.  This is necessarily subjective.
;;
;;     Unicode also defines the notion of a "script" as a higher-level
;;     abstraction which is independent of "blocks".  Modern fonts can
;;     report their script coverage, and Emacs may also access that
;;     information.  However, this library ignores scripts in favor
;;     of blocks and glyphs.
;;
;;     Checking for font availability is slow.  This library can
;;     add anywhere between 0.1 - 10 secs to startup time.  It is
;;     slowest under X11.  Some per-architecture limitations are
;;     documented in font-utils.el
;;
;;     Calling `set-fontset-font' can easily crash Emacs.  There is a
;;     workaround, but it may not be sufficient on all platforms.
;;     Tested on Cocoa Emacs, Native Mac Emacs, X11/XQuartz,
;;     MS Windows XP.
;;
;;     Glyph-by-glyph fallthrough happens differently depending on the
;;     font backend.  On Cocoa Emacs, glyph-by-glyph fallthrough does not
;;     occur, and manual per-glyph overrides are required to maximize
;;     coverage.  Fallthrough works on MS Windows, but not perfectly.
;;     X11/FreeType behaves most predictably.
;;
;;     The following ranges cannot be overridden within the
;;     "fontset-default" fontset:
;;
;;         Latin Extended Additional
;;         Latin Extended-B
;;         Spacing Modifier Letters
;;
;;     `unicode-fonts-overrides-mapping' shows some order-dependence,
;;     which must indicate a bug in this code.
;;
;;     A number of the entries in `unicode-fonts-overrides-mapping'
;;     are workarounds for the font Monaco, and therefore specific
;;     to OS X.
;;
;;     Widths of alternate fonts do not act as expected on MS Windows.
;;     For example, DejaVu Sans Mono box-drawing characters may use
;;     a different width than the default font.
;;
;; TODO
;;
;;     provide additional interfaces
;;     - dump set-fontset-font instructions
;;     - immediately set font for character/current-character/range
;;     - recommend font for current character
;;     - alternatives to customize, which can be called before unicode-fonts-setup
;;       - eg "prefer this font for this block"
;;       - also character/range ie overrides
;;
;;     scripts vs blocks
;;     - further doc note
;;     - provide alternative interface via scripts
;;
;;     reorganize font list by language?
;;     - break down into living/dead/invented
;;
;;     support MUFI for PUA
;;
;;     support ConScript for PUA
;;
;;     Aramaic as a style of Hebrew
;;
;;     (set-language-environment "UTF-8") ?
;;
;;     Include all Windows 8 fonts
;;
;;     Include all Windows 10 fonts
;;
;;     Remove very old Microsoft entries (eg Monotype.com which was
;;     renamed Andale)
;;
;;     Recognize the default font and make smarter choices when it is
;;     one of the provided mappings.  (On Cocoa, the default font is
;;     returned when font-info fails, which is not a good thing
;;     overall.)
;;
;;     For every font, list font version and unicode blocks which are
;;     complete.
;;
;;     Note all decorative fonts
;;
;;     Adobe international fonts which are supplied with Reader
;;
;;     Apple fonts which could not be mapped
;;         Wawati TC
;;         Weibei TC
;;         Weibei SC
;;         Wawati SC
;;
;;; License
;;
;; Simplified BSD License:
;;
;; Redistribution and use in source and binary forms, with or
;; without modification, are permitted provided that the following
;; conditions are met:
;;
;;   1. Redistributions of source code must retain the above
;;      copyright notice, this list of conditions and the following
;;      disclaimer.
;;
;;   2. Redistributions in binary form must reproduce the above
;;      copyright notice, this list of conditions and the following
;;      disclaimer in the documentation and/or other materials
;;      provided with the distribution.
;;
;; This software is provided by Roland Walker "AS IS" and any express
;; or implied warranties, including, but not limited to, the implied
;; warranties of merchantability and fitness for a particular
;; purpose are disclaimed.  In no event shall Roland Walker or
;; contributors be liable for any direct, indirect, incidental,
;; special, exemplary, or consequential damages (including, but not
;; limited to, procurement of substitute goods or services; loss of
;; use, data, or profits; or business interruption) however caused
;; and on any theory of liability, whether in contract, strict
;; liability, or tort (including negligence or otherwise) arising in
;; any way out of the use of this software, even if advised of the
;; possibility of such damage.
;;
;; The views and conclusions contained in the software and
;; documentation are those of the authors and should not be
;; interpreted as representing official policies, either expressed
;; or implied, of Roland Walker.
;;
;; No rights are claimed over data created by the Unicode
;; Consortium, which are included here under the terms of
;; the Unicode Terms of Use.
;;
;;; Code:
;;

;;; requirements

;; for callf, callf2, member*, incf, remove-if, remove-if-not
(require 'cl)

(autoload 'persistent-soft-store               "persistent-soft" "Under SYMBOL, store VALUE in the LOCATION persistent data store."    )
(autoload 'persistent-soft-fetch               "persistent-soft" "Return the value for SYMBOL in the LOCATION persistent data store."  )
(autoload 'persistent-soft-exists-p            "persistent-soft" "Return t if SYMBOL exists in the LOCATION persistent data store."    )
(autoload 'persistent-soft-flush               "persistent-soft" "Flush data for the LOCATION data store to disk."                     )
(autoload 'persistent-soft-location-readable   "persistent-soft" "Return non-nil if LOCATION is a readable persistent-soft data store.")
(autoload 'persistent-soft-location-destroy    "persistent-soft" "Destroy LOCATION (a persistent-soft data store)."                    )

(autoload 'font-utils-exists-p                 "font-utils"  "Test whether FONT-NAME (a string or font object) exists.")
(autoload 'font-utils-read-name                "font-utils"  "Read a font name using `completing-read'.")
(autoload 'font-utils-lenient-name-equal       "font-utils"  "Leniently match two strings, FONT-NAME-A and FONT-NAME-B.")
(autoload 'font-utils-first-existing-font      "font-utils"  "Return the (normalized) first existing font name from FONT-NAMES.")
(autoload 'font-utils-name-from-xlfd           "font-utils"  "Return the font-family name from XLFD, a string.")
(autoload 'font-utils-is-qualified-variant     "font-utils"  "Test whether FONT-NAME-1 and FONT-NAME-2 are qualified variants of the same font.")
(autoload 'font-utils-list-names               "font-utils"  "Return a list of all font names on the current system.")
(autoload 'font-utils-client-hostname          "font-utils"  "Guess the client hostname, respecting $SSH_CONNECTION.")

(autoload 'ucs-utils-char                      "ucs-utils"   "Return the character corresponding to NAME, a UCS name.")
(autoload 'ucs-utils-pretty-name               "ucs-utils"   "Return a prettified UCS name for CHAR.")

;;; constants

(defconst unicode-fonts-planes
  '(("unicode-bmp"         #x0000    #xFFFF)            ; plane  0
    ("unicode-smp"         #x10000   #x1FFFF)           ; plane  1
    ("unicode-sip"         #x20000   #x2FFFF)           ; plane  2
    ("unicode-unassigned"  #x30000   #xDFFFF)           ; planes 3-13
    ("unicode-ssp"         #xE0000   #xEFFFF)           ; plane  14
    ("unicode-pua-a"       #xF0000   #xFFFFF)           ; plane  15
    ("unicode-pua-b"       #x100000  #x10FFFF))         ; plane  16
  "Alist of Unicode 8.0 planes.")

(defconst unicode-fonts-blocks
  '(("Aegean Numbers"                                  #x10100  #x1013F)
    ("Ahom"                                            #x11700  #x1173F)
    ("Alchemical Symbols"                              #x1F700  #x1F77F)
    ("Alphabetic Presentation Forms"                   #xFB00   #xFB4F)
    ("Anatolian Hieroglyphs"                           #x14400  #x1467F)
    ("Ancient Greek Musical Notation"                  #x1D200  #x1D24F)
    ("Ancient Greek Numbers"                           #x10140  #x1018F)
    ("Ancient Symbols"                                 #x10190  #x101CF)
    ("Arabic"                                          #x0600   #x06FF)
    ("Arabic Extended-A"                               #x08A0   #x08FF)
    ("Arabic Mathematical Alphabetic Symbols"          #x1EE00  #x1EEFF)
    ("Arabic Presentation Forms-A"                     #xFB50   #xFDFF)
    ("Arabic Presentation Forms-B"                     #xFE70   #xFEFF)
    ("Arabic Supplement"                               #x0750   #x077F)
    ("Armenian"                                        #x0530   #x058F)
    ("Arrows"                                          #x2190   #x21FF)
    ("Avestan"                                         #x10B00  #x10B3F)
    ("Balinese"                                        #x1B00   #x1B7F)
    ("Bamum"                                           #xA6A0   #xA6FF)
    ("Bamum Supplement"                                #x16800  #x16A3F)
    ("Basic Latin"                                     #x0000   #x007F)
    ("Bassa Vah"                                       #x16AD0  #x16AFF)
    ("Batak"                                           #x1BC0   #x1BFF)
    ("Bengali"                                         #x0980   #x09FF)
    ("Block Elements"                                  #x2580   #x259F)
    ("Bopomofo"                                        #x3100   #x312F)
    ("Bopomofo Extended"                               #x31A0   #x31BF)
    ("Box Drawing"                                     #x2500   #x257F)
    ("Brahmi"                                          #x11000  #x1107F)
    ("Braille Patterns"                                #x2800   #x28FF)
    ("Buginese"                                        #x1A00   #x1A1F)
    ("Buhid"                                           #x1740   #x175F)
    ("Byzantine Musical Symbols"                       #x1D000  #x1D0FF)
    ("Carian"                                          #x102A0  #x102DF)
    ("Caucasian Albanian"                              #x10530  #x1056F)
    ("Chakma"                                          #x11100  #x1114F)
    ("Cham"                                            #xAA00   #xAA5F)
    ("Cherokee"                                        #x13A0   #x13FF)
    ("Cherokee Supplement"                             #xAB70   #xABBF)
    ("CJK Compatibility"                               #x3300   #x33FF)
    ("CJK Compatibility Forms"                         #xFE30   #xFE4F)
    ("CJK Compatibility Ideographs"                    #xF900   #xFAFF)
    ("CJK Compatibility Ideographs Supplement"         #x2F800  #x2FA1F)
    ("CJK Radicals Supplement"                         #x2E80   #x2EFF)
    ("CJK Strokes"                                     #x31C0   #x31EF)
    ("CJK Symbols and Punctuation"                     #x3000   #x303F)
    ("CJK Unified Ideographs"                          #x4E00   #x9FFF)
    ("CJK Unified Ideographs Extension A"              #x3400   #x4DBF)
    ("CJK Unified Ideographs Extension B"              #x20000  #x2A6DF)
    ("CJK Unified Ideographs Extension C"              #x2A700  #x2B73F)
    ("CJK Unified Ideographs Extension D"              #x2B740  #x2B81F)
    ("CJK Unified Ideographs Extension E"              #x2B820  #x2CEAF)
    ("Combining Diacritical Marks"                     #x0300   #x036F)
    ("Combining Diacritical Marks Extended"            #x1AB0   #x1AFF)
    ("Combining Diacritical Marks Supplement"          #x1DC0   #x1DFF)
    ("Combining Diacritical Marks for Symbols"         #x20D0   #x20FF)
    ("Combining Half Marks"                            #xFE20   #xFE2F)
    ("Common Indic Number Forms"                       #xA830   #xA83F)
    ("Control Pictures"                                #x2400   #x243F)
    ("Coptic"                                          #x2C80   #x2CFF)
    ("Coptic Epact Numbers"                            #x102E0  #x102FF)
    ("Counting Rod Numerals"                           #x1D360  #x1D37F)
    ("Cuneiform"                                       #x12000  #x123FF)
    ("Cuneiform Numbers and Punctuation"               #x12400  #x1247F)
    ("Currency Symbols"                                #x20A0   #x20CF)
    ("Cypriot Syllabary"                               #x10800  #x1083F)
    ("Cyrillic"                                        #x0400   #x04FF)
    ("Cyrillic Extended-A"                             #x2DE0   #x2DFF)
    ("Cyrillic Extended-B"                             #xA640   #xA69F)
    ("Cyrillic Supplement"                             #x0500   #x052F)
    ("Deseret"                                         #x10400  #x1044F)
    ("Devanagari"                                      #x0900   #x097F)
    ("Devanagari Extended"                             #xA8E0   #xA8FF)
    ("Dingbats"                                        #x2700   #x27BF)
    ("Domino Tiles"                                    #x1F030  #x1F09F)
    ("Duployan"                                        #x1BC00  #x1BC9F)
    ("Early Dynastic Cuneiform"                        #x12480  #x1254F)
    ("Egyptian Hieroglyphs"                            #x13000  #x1342F)
    ("Elbasan"                                         #x10500  #x1052F)
    ("Emoticons"                                       #x1F600  #x1F64F)
    ("Enclosed Alphanumeric Supplement"                #x1F100  #x1F1FF)
    ("Enclosed Alphanumerics"                          #x2460   #x24FF)
    ("Enclosed CJK Letters and Months"                 #x3200   #x32FF)
    ("Enclosed Ideographic Supplement"                 #x1F200  #x1F2FF)
    ("Ethiopic"                                        #x1200   #x137F)
    ("Ethiopic Extended"                               #x2D80   #x2DDF)
    ("Ethiopic Extended-A"                             #xAB00   #xAB2F)
    ("Ethiopic Supplement"                             #x1380   #x139F)
    ("General Punctuation"                             #x2000   #x206F)
    ("Geometric Shapes"                                #x25A0   #x25FF)
    ("Geometric Shapes Extended"                       #x1F780  #x1F7FF)
    ("Georgian"                                        #x10A0   #x10FF)
    ("Georgian Supplement"                             #x2D00   #x2D2F)
    ("Glagolitic"                                      #x2C00   #x2C5F)
    ("Gothic"                                          #x10330  #x1034F)
    ("Grantha"                                         #x11300  #x1137F)
    ("Greek Extended"                                  #x1F00   #x1FFF)
    ("Greek and Coptic"                                #x0370   #x03FF)
    ("Gujarati"                                        #x0A80   #x0AFF)
    ("Gurmukhi"                                        #x0A00   #x0A7F)
    ("Halfwidth and Fullwidth Forms"                   #xFF00   #xFFEF)
    ("Hangul Compatibility Jamo"                       #x3130   #x318F)
    ("Hangul Jamo"                                     #x1100   #x11FF)
    ("Hangul Jamo Extended-A"                          #xA960   #xA97F)
    ("Hangul Jamo Extended-B"                          #xD7B0   #xD7FF)
    ("Hangul Syllables"                                #xAC00   #xD7AF)
    ("Hanunoo"                                         #x1720   #x173F)
    ("Hatran"                                          #x108E0  #x108FF)
    ("Hebrew"                                          #x0590   #x05FF)
    ;; ("High Private Use Surrogates"                  #xDB80   #xDBFF) ; no displayable characters
    ;; ("High Surrogates"                              #xD800   #xDB7F) ; no displayable characters
    ("Hiragana"                                        #x3040   #x309F)
    ("Ideographic Description Characters"              #x2FF0   #x2FFF)
    ("Imperial Aramaic"                                #x10840  #x1085F)
    ("Inscriptional Pahlavi"                           #x10B60  #x10B7F)
    ("Inscriptional Parthian"                          #x10B40  #x10B5F)
    ("IPA Extensions"                                  #x0250   #x02AF)
    ("Javanese"                                        #xA980   #xA9DF)
    ("Kaithi"                                          #x11080  #x110CF)
    ("Kana Supplement"                                 #x1B000  #x1B0FF)
    ("Kanbun"                                          #x3190   #x319F)
    ("Kangxi Radicals"                                 #x2F00   #x2FDF)
    ("Kannada"                                         #x0C80   #x0CFF)
    ("Katakana"                                        #x30A0   #x30FF)
    ("Katakana Phonetic Extensions"                    #x31F0   #x31FF)
    ("Kayah Li"                                        #xA900   #xA92F)
    ("Kharoshthi"                                      #x10A00  #x10A5F)
    ("Khmer"                                           #x1780   #x17FF)
    ("Khmer Symbols"                                   #x19E0   #x19FF)
    ("Khojki"                                          #x11200  #x1124F)
    ("Khudawadi"                                       #x112B0  #x112FF)
    ("Lao"                                             #x0E80   #x0EFF)
    ("Latin Extended Additional"                       #x1E00   #x1EFF)
    ("Latin Extended-A"                                #x0100   #x017F)
    ("Latin Extended-B"                                #x0180   #x024F)
    ("Latin Extended-C"                                #x2C60   #x2C7F)
    ("Latin Extended-D"                                #xA720   #xA7FF)
    ("Latin Extended-E"                                #xAB30   #xAB6F)
    ("Latin-1 Supplement"                              #x0080   #x00FF)
    ("Lepcha"                                          #x1C00   #x1C4F)
    ("Letterlike Symbols"                              #x2100   #x214F)
    ("Limbu"                                           #x1900   #x194F)
    ("Linear A"                                        #x10600  #x1077F)
    ("Linear B Ideograms"                              #x10080  #x100FF)
    ("Linear B Syllabary"                              #x10000  #x1007F)
    ("Lisu"                                            #xA4D0   #xA4FF)
    ;; ("Low Surrogates"                               #xDC00   #xDFFF) ; no displayable characters
    ("Lycian"                                          #x10280  #x1029F)
    ("Lydian"                                          #x10920  #x1093F)
    ("Mahajani"                                        #x11150  #x1117F)
    ("Mahjong Tiles"                                   #x1F000  #x1F02F)
    ("Malayalam"                                       #x0D00   #x0D7F)
    ("Mandaic"                                         #x0840   #x085F)
    ("Manichaean"                                      #x10AC0  #x10AFF)
    ("Mathematical Alphanumeric Symbols"               #x1D400  #x1D7FF)
    ("Mathematical Operators"                          #x2200   #x22FF)
    ("Meetei Mayek"                                    #xABC0   #xABFF)
    ("Meetei Mayek Extensions"                         #xAAE0   #xAAFF)
    ("Mende Kikakui"                                   #x1E800  #x1E8DF)
    ("Meroitic Cursive"                                #x109A0  #x109FF)
    ("Meroitic Hieroglyphs"                            #x10980  #x1099F)
    ("Miao"                                            #x16F00  #x16F9F)
    ("Miscellaneous Mathematical Symbols-A"            #x27C0   #x27EF)
    ("Miscellaneous Mathematical Symbols-B"            #x2980   #x29FF)
    ("Miscellaneous Symbols"                           #x2600   #x26FF)
    ("Miscellaneous Symbols and Arrows"                #x2B00   #x2BFF)
    ("Miscellaneous Symbols and Pictographs"           #x1F300  #x1F5FF)
    ("Miscellaneous Technical"                         #x2300   #x23FF)
    ("Modi"                                            #x11600  #x1165F)
    ("Modifier Tone Letters"                           #xA700   #xA71F)
    ("Mongolian"                                       #x1800   #x18AF)
    ("Mro"                                             #x16A40  #x16A6F)
    ("Multani"                                         #x11280  #x112AF)
    ("Musical Symbols"                                 #x1D100  #x1D1FF)
    ("Myanmar"                                         #x1000   #x109F)
    ("Myanmar Extended-A"                              #xAA60   #xAA7F)
    ("Myanmar Extended-B"                              #xA9E0   #xA9FF)
    ("Nabataean"                                       #x10880  #x108AF)
    ("New Tai Lue"                                     #x1980   #x19DF)
    ("NKo"                                             #x07C0   #x07FF)
    ("Number Forms"                                    #x2150   #x218F)
    ("Ogham"                                           #x1680   #x169F)
    ("Ol Chiki"                                        #x1C50   #x1C7F)
    ("Old Hungarian"                                   #x10C80  #x10CFF)
    ("Old Italic"                                      #x10300  #x1032F)
    ("Old North Arabian"                               #x10A80  #x10A9F)
    ("Old Permic"                                      #x10350  #x1037F)
    ("Old Persian"                                     #x103A0  #x103DF)
    ("Old South Arabian"                               #x10A60  #x10A7F)
    ("Old Turkic"                                      #x10C00  #x10C4F)
    ("Optical Character Recognition"                   #x2440   #x245F)
    ("Oriya"                                           #x0B00   #x0B7F)
    ("Ornamental Dingbats"                             #x1F650  #x1F67F)
    ("Osmanya"                                         #x10480  #x104AF)
    ("Pahawh Hmong"                                    #x16B00  #x16B8F)
    ("Palmyrene"                                       #x10860  #x1087F)
    ("Pau Cin Hau"                                     #x11AC0  #x11AFF)
    ("Phags-pa"                                        #xA840   #xA87F)
    ("Phaistos Disc"                                   #x101D0  #x101FF)
    ("Phoenician"                                      #x10900  #x1091F)
    ("Phonetic Extensions"                             #x1D00   #x1D7F)
    ("Phonetic Extensions Supplement"                  #x1D80   #x1DBF)
    ("Playing Cards"                                   #x1F0A0  #x1F0FF)
    ("Private Use Area"                                #xE000   #xF8FF)
    ("Psalter Pahlavi"                                 #x10B80  #x10BAF)
    ("Rejang"                                          #xA930   #xA95F)
    ("Rumi Numeral Symbols"                            #x10E60  #x10E7F)
    ("Runic"                                           #x16A0   #x16FF)
    ("Samaritan"                                       #x0800   #x083F)
    ("Saurashtra"                                      #xA880   #xA8DF)
    ("Sharada"                                         #x11180  #x111DF)
    ("Shavian"                                         #x10450  #x1047F)
    ;; ("Shorthand Format Controls"                    #x1BCA0  #x1BCAF) ; no displayable characters
    ("Siddham"                                         #x11580  #x115FF)
    ("Sinhala"                                         #x0D80   #x0DFF)
    ("Sinhala Archaic Numbers"                         #x111E0  #x111FF)
    ("Small Form Variants"                             #xFE50   #xFE6F)
    ("Sora Sompeng"                                    #x110D0  #x110FF)
    ("Spacing Modifier Letters"                        #x02B0   #x02FF)
    ("Specials"                                        #xFFF0   #xFFFF)
    ("Sundanese"                                       #x1B80   #x1BBF)
    ("Sundanese Supplement"                            #x1CC0   #x1CCF)
    ("Superscripts and Subscripts"                     #x2070   #x209F)
    ("Supplemental Arrows-A"                           #x27F0   #x27FF)
    ("Supplemental Arrows-B"                           #x2900   #x297F)
    ("Supplemental Arrows-C"                           #x1F800  #x1F8FF)
    ("Supplemental Mathematical Operators"             #x2A00   #x2AFF)
    ("Supplemental Punctuation"                        #x2E00   #x2E7F)
    ("Supplemental Symbols and Pictographs"            #x1F900  #x1F9FF)
    ("Supplementary Private Use Area-A"                #xF0000  #xFFFFD)
    ("Supplementary Private Use Area-B"                #x100000 #x10FFFD)
    ("Sutton SignWriting"                              #x1D800  #x1DAAF)
    ("Syloti Nagri"                                    #xA800   #xA82F)
    ("Syriac"                                          #x0700   #x074F)
    ("Tagalog"                                         #x1700   #x171F)
    ("Tagbanwa"                                        #x1760   #x177F)
    ("Tags"                                            #xE0000  #xE007F)
    ("Tai Le"                                          #x1950   #x197F)
    ("Tai Tham"                                        #x1A20   #x1AAF)
    ("Tai Viet"                                        #xAA80   #xAADF)
    ("Tai Xuan Jing Symbols"                           #x1D300  #x1D35F)
    ("Takri"                                           #x11680  #x116CF)
    ("Tamil"                                           #x0B80   #x0BFF)
    ("Telugu"                                          #x0C00   #x0C7F)
    ("Thaana"                                          #x0780   #x07BF)
    ("Thai"                                            #x0E00   #x0E7F)
    ("Tibetan"                                         #x0F00   #x0FFF)
    ("Tifinagh"                                        #x2D30   #x2D7F)
    ("Tirhuta"                                         #x11480  #x114DF)
    ("Transport and Map Symbols"                       #x1F680  #x1F6FF)
    ("Ugaritic"                                        #x10380  #x1039F)
    ("Unified Canadian Aboriginal Syllabics"           #x1400   #x167F)
    ("Unified Canadian Aboriginal Syllabics Extended"  #x18B0   #x18FF)
    ("Vai"                                             #xA500   #xA63F)
    ("Variation Selectors"                             #xFE00   #xFE0F)
    ("Variation Selectors Supplement"                  #xE0100  #xE01EF)
    ("Vedic Extensions"                                #x1CD0   #x1CFF)
    ("Vertical Forms"                                  #xFE10   #xFE1F)
    ("Warang Citi"                                     #x118A0  #x118FF)
    ("Yi Radicals"                                     #xA490   #xA4CF)
    ("Yi Syllables"                                    #xA000   #xA48F)
    ("Yijing Hexagram Symbols"                         #x4DC0   #x4DFF))
  "Alist of Unicode 8.0 blocks.")

(defvar unicode-fonts-known-font-characteristics
  '(("Abadi MT Condensed"                  :licenses (microsoft))
    ("Aboriginal Sans"                     :licenses (free))
    ("Aboriginal Serif"                    :licenses (free))
    ("Abyssinica SIL"                      :licenses (free))
    ("Adinatha Tamil Brahmi"               :licenses (free))
    ("Adobe Arabic"                        :licenses (adobe) :arabic naskh)
    ("Adobe Hebrew"                        :licenses (adobe))
    ("Adobe Minion Web"                    :licenses (microsoft))
    ("African Sans"                        :licenses (free))
    ("African Serif"                       :licenses (free))
    ("Aldhabi"                             :licenses (microsoft) :arabic naskh)
    ("Albanian"                            :licenses (free))
    ("Aegean"                              :licenses (free))
    ("Aegyptus"                            :licenses (free))
    ("Agency FB"                           :licenses (microsoft))
    ("AhomUnicode"                         :licenses (free))
    ("Aharoni"                             :licenses (microsoft))
    ("Ahuramzda"                           :licenses (free))
    ("Akaash"                              :licenses (free))
    ("Akkadian"                            :licenses (free))
    ("Aksara Bali"                         :licenses (free))
    ("Akshar Unicode"                      :licenses (free))
    ("Al Bayan"                            :licenses (apple) :arabic naskh)
    ("Aleem Urdu Unicode"                  :licenses (free) :arabic urdu)
    ("Algerian"                            :licenses (microsoft))
    ("Almanac MT"                          :licenses (microsoft))
    ("ALPHABETUM Unicode"                  :licenses (commercial))
    ("American Typewriter"                 :licenses (apple))
    ("American Uncial"                     :licenses (microsoft))
    ("Amiri"                               :licenses (free) :arabic naskh)
    ("Analecta"                            :licenses (free))
    ("Anatolian"                           :licenses (free))
    ("Andagii"                             :licenses (free))
    ("Andale Mono"                         :spacing mono :licenses (apple microsoft))
    ("Andalus"                             :licenses (microsoft))
    ("Andy"                                :licenses (microsoft))
    ("Angsana New"                         :licenses (microsoft))
    ("AngsanaUPC"                          :licenses (microsoft))
    ("AnmolUni"                            :licenses (free))
    ("Annapurna SIL"                       :licenses (free))
    ("Antinoou"                            :licenses (free))
    ("Aparajita"                           :licenses (microsoft))
    ("Apple Braille"                       :licenses (apple))
    ("Apple Casual"                        :licenses (apple))
    ("Apple Chancery"                      :licenses (apple))
    ("Apple Color Emoji"                   :licenses (apple) :color multi)
    ("AppleGothic"                         :licenses (apple))
    ("Apple LiGothic"                      :licenses (apple) :chinese traditional)
    ("Apple LiSung"                        :licenses (apple) :chinese traditional)
    ("AppleMyungjo"                        :licenses (apple) :glyph-quality low)
    ("Apple SD Gothic Neo"                 :licenses (apple))
    ("Apple Symbols"                       :licenses (apple))
    ("Arabic Transparent"                  :licenses (microsoft) :arabic naskh)
    ("Arabic Typesetting"                  :licenses (microsoft) :arabic naskh)
    ("Aramaic Imperial Yeb"                :licenses (free))
    ("Arial Black"                         :licenses (apple microsoft))
    ("Arial Hebrew"                        :licenses (apple))
    ("Arial Narrow Special"                :licenses (microsoft))
    ("Arial Narrow"                        :licenses (apple microsoft))
    ("Arial Rounded MT Bold"               :licenses (apple microsoft))
    ("Arial Special"                       :licenses (microsoft))
    ("Arial Unicode MS"                    :licenses (apple microsoft) :arabic naskh)
    ("Arial"                               :licenses (apple microsoft))
    ("Asana Math"                          :licenses (free))
    ("Augsburger Initials"                 :licenses (microsoft))
    ("Avenir Next Condensed"               :licenses (apple))
    ("Avenir Next"                         :licenses (apple))
    ("Avenir"                              :licenses (apple))
    ("Ayuthaya"                            :licenses (apple))
    ("BabelStone Han"                      :chinese simplified :licenses (free))
    ("BabelStone Modern"                   :licenses (free))
    ("BabelStone Phags-pa Book"            :licenses (free))
    ("Baghdad"                             :licenses (apple) :arabic naskh)
    ("Bangla MN"                           :licenses (apple))
    ("Bangla Sangam MN"                    :licenses (apple))
    ("Baoli SC"                            :licenses (apple) :chinese simplified)
    ("Baskerville Old Face"                :licenses (microsoft))
    ("Baskerville"                         :licenses (apple))
    ("Batak-Unicode"                       :licenses (free))
    ("Batang"                              :licenses (microsoft))
    ("Bauhaus 93"                          :licenses (microsoft))
    ("Beesknees ITC"                       :licenses (microsoft))
    ("Bell MT"                             :licenses (microsoft))
    ("Berlin Sans FB"                      :licenses (microsoft))
    ("Bernard MT Condensed"                :licenses (microsoft))
    ("BiauKai"                             :licenses (apple) :chinese traditional)
    ("Bickley Script"                      :licenses (microsoft))
    ("Big Caslon"                          :licenses (apple))
    ("Blackadder ITC"                      :licenses (microsoft))
    ("Bodoni MT Condensed"                 :licenses (microsoft))
    ("Bodoni MT"                           :licenses (microsoft))
    ("Bon Apetit MT"                       :licenses (microsoft))
    ("Book Antiqua"                        :licenses (microsoft))
    ("Bookman Old Style"                   :licenses (microsoft))
    ("Bookshelf Symbol 7"                  :licenses (microsoft))
    ("Bradley Hand ITC"                    :licenses (microsoft))
    ("Braggadocio"                         :licenses (microsoft))
    ("BriemScript"                         :licenses (microsoft))
    ("Britannic Bold"                      :licenses (microsoft))
    ("Broadway"                            :licenses (microsoft))
    ("Browallia New"                       :licenses (microsoft))
    ("BrowalliaUPC"                        :licenses (microsoft))
    ("Brush Script MT"                     :licenses (microsoft))
    ("Brush Script Std"                    :licenses (apple))
    ("Calibri"                             :licenses (microsoft) :cleartype t)
    ("Californian FB"                      :licenses (microsoft))
    ("Calisto MT"                          :licenses (microsoft))
    ("Cambria Math"                        :licenses (microsoft) :cleartype t :buggy-before-vista t)
    ("Cambria"                             :licenses (microsoft))
    ("Candara"                             :licenses (microsoft))
    ("Cardo"                               :licenses (free))
    ("Cariadings"                          :licenses (microsoft))
    ("Castellar"                           :licenses (microsoft))
    ("Centaur"                             :licenses (microsoft))
    ("Century Gothic"                      :licenses (microsoft))
    ("Century Schoolbook"                  :licenses (microsoft))
    ("Century"                             :licenses (microsoft))
    ("Chalkboard SE"                       :licenses (apple))
    ("Chalkboard"                          :licenses (apple))
    ("Chalkduster"                         :licenses (apple))
    ("Cham OI_Kul"                         :licenses (free))
    ("Cham OI_Kulbleng"                    :licenses (free))
    ("Cham OI_Tangin"                      :licenses (free))
    ("Charcoal CY"                         :licenses (apple))
    ("Charis SIL"                          :licenses (free))
    ("Chiangsaen Alif"                     :licenses (free))
    ("Chiller"                             :licenses (microsoft))
    ("Cochin"                              :licenses (apple))
    ("Code2000"                            :licenses (unclear))
    ("Code2001"                            :licenses (free))
    ("Code2002"                            :licenses (free))
    ("Colonna MT"                          :licenses (microsoft))
    ("Comic Sans MS"                       :licenses (apple microsoft))
    ("Conakry"                             :licenses (free))
    ("Consolas"                            :spacing mono :licenses (microsoft) :cleartype t)
    ("Constantia"                          :licenses (microsoft))
    ("Contemporary Brush"                  :licenses (microsoft))
    ("Cooper Black"                        :licenses (microsoft))
    ("Cooper Std"                          :licenses (apple))
    ("Copperplate Gothic"                  :licenses (microsoft))
    ("Copperplate"                         :licenses (apple))
    ("Corbel"                              :licenses (microsoft))
    ("Cordia New"                          :licenses (microsoft))
    ("CordiaUPC"                           :licenses (microsoft))
    ("Corsiva Hebrew"                      :licenses (apple))
    ("Courier New"                         :spacing mono :licenses (apple microsoft))
    ("Courier"                             :licenses (apple))
    ("Curlz MT"                            :licenses (microsoft))
    ("Dai Banna SIL Book"                  :licenses (free))
    ("Dai Banna SIL Light"                 :licenses (free))
    ("Daicing Xiaokai"                     :licenses (free))
    ("Damascus"                            :licenses (apple))
    ("DaunPenh"                            :licenses (microsoft))
    ("David"                               :licenses (microsoft))
    ("DecoType Naskh"                      :licenses (apple) :arabic naskh)
    ("DejaVu Sans Mono"                    :spacing mono :licenses (free) :arabic naskh)
    ("DejaVu Sans"                         :licenses (free) :arabic naskh)
    ("DejaVu Serif"                        :licenses (free) :arabic naskh)
    ("Desdemona"                           :licenses (microsoft))
    ("Devanagari MT"                       :licenses (apple))
    ("Devanagari Sangam MN"                :licenses (apple))
    ("DFKai-SB"                            :chinese traditional :licenses (microsoft))
    ("Didot"                               :licenses (apple))
    ("Digohweli"                           :licenses (free))
    ("Digohweli Old DO"                    :licenses (free))
    ("DilleniaUPC"                         :licenses (microsoft))
    ("Directions MT"                       :licenses (microsoft))
    ("Diwani Letter"                       :licenses (microsoft) :arabic diwani)
    ("DokChampa"                           :licenses (microsoft))
    ("Dotum"                               :licenses (microsoft))
    ("Doulos SIL"                          :licenses (free))
    ("Dukor"                               :licenses (free))
    ("Ebrima"                              :licenses (microsoft))
    ("Eckmann"                             :licenses (microsoft))
    ("Edda"                                :licenses (microsoft))
    ("Edwardian Script ITC"                :licenses (microsoft))
    ("Eeyek Unicode"                       :licenses (free))
    ("Elephant"                            :licenses (microsoft))
    ("Elham"                               :licenses (free) :arabic farsi)
    ("Engravers MT"                        :licenses (microsoft))
    ("Engravers"                           :licenses (microsoft))
    ("Enviro"                              :licenses (microsoft))
    ("Eras ITC"                            :licenses (microsoft))
    ("Estrangelo Edessa"                   :licenses (microsoft free))
    ("Estrangelo Nisibin"                  :licenses (free))
    ("Ethiopia Jiret"                      :licenses (free))
    ("Ethiopic WashRa SemiBold"            :licenses (free))
    ("Ethiopic Yebse"                      :licenses (free))
    ("EucrosiaUPC"                         :licenses (microsoft))
    ("Euphemia UCAS"                       :licenses (apple))
    ("Euphemia"                            :licenses (microsoft))
    ("Eurostile"                           :licenses (microsoft))
    ("Everson Mono"                        :spacing mono :licenses (commercial))
    ("Ezra SIL"                            :licenses (free))
    ("Ezra SIL SR"                         :licenses (free))
    ("FangSong"                            :spacing mono :chinese simplified :licenses (microsoft))
    ("Farsi Simple Bold"                   :licenses (microsoft) :arabic farsi)
    ("Felix Titling"                       :licenses (microsoft))
    ("Fine Hand"                           :licenses (microsoft))
    ("Fixed Miriam Transparent"            :licenses (microsoft))
    ("Flexure"                             :licenses (microsoft))
    ("Footlight MT Light"                  :licenses (microsoft))
    ("Footlight MT"                        :licenses (microsoft))
    ("Forte"                               :licenses (microsoft))
    ("FrankRuehl"                          :licenses (microsoft))
    ("Franklin Gothic"                     :licenses (microsoft))
    ("FreeMono"                            :licenses (free))
    ("FreeSans"                            :licenses (free))
    ("FreeSerif"                           :licenses (free))
    ("FreesiaUPC"                          :licenses (microsoft))
    ("Freestyle Script"                    :licenses (microsoft))
    ("French Script MT"                    :licenses (microsoft))
    ("Futura"                              :licenses (microsoft apple))
    ("Gabriola"                            :licenses (microsoft))
    ("Gadugi"                              :licenses (microsoft))
    ("Garamond MT"                         :licenses (microsoft))
    ("Garamond"                            :licenses (microsoft))
    ("Gardiner"                            :licenses (free))
    ("Gautami"                             :licenses (microsoft))
    ("GB18030 Bitmap"                      :licenses (apple) :chinese simplified :glyph-quality low)
    ("Geeza Pro"                           :licenses (apple) :arabic naskh)
    ("Geneva CY"                           :licenses (apple))
    ("Geneva"                              :licenses (apple))
    ("Gentium Plus Compact"                :licenses (free))
    ("Gentium Plus"                        :licenses (free))
    ("Georgia Ref"                         :licenses (microsoft))
    ("Georgia"                             :licenses (apple microsoft))
    ("Gigi"                                :licenses (microsoft))
    ("Gill Sans MT Condensed"              :licenses (microsoft))
    ("Gill Sans MT"                        :licenses (microsoft))
    ("Gill Sans"                           :licenses (apple))
    ("Gisha"                               :licenses (microsoft))
    ("Gloucester MT Extra Condensed"       :licenses (microsoft))
    ("Gloucester"                          :licenses (microsoft))
    ("Goudy Old Style"                     :licenses (microsoft))
    ("Goudy Stout"                         :licenses (microsoft))
    ("Gradl"                               :licenses (microsoft))
    ("Gujarati MT"                         :licenses (apple))
    ("Gujarati Sangam MN"                  :licenses (apple))
    ("Gulim"                               :licenses (microsoft))
    ("GungSeo"                             :licenses (apple))
    ("Gungsuh"                             :licenses (microsoft))
    ("Gurmukhi MN"                         :licenses (apple))
    ("Gurmukhi Sangam MN"                  :licenses (apple))
    ("Hacen Sudan"                         :licenses (free))
    ("Haettenschweiler"                    :licenses (microsoft))
    ("HAN NOM A"                           :licenses (free) :chinese nom)
    ("HAN NOM B"                           :licenses (free) :chinese nom)
    ("HanaMinA"                            :licenses (free) :chinese traditional)
    ("HanaMinB"                            :licenses (free) :chinese traditional)
    ("Harlow Solid"                        :licenses (microsoft))
    ("Harrington"                          :licenses (microsoft))
    ("HeadLineA"                           :licenses (apple) decorative t)
    ("Hei"                                 :licenses (apple) :chinese simplified)
    ("Heiti SC"                            :licenses (apple) :chinese simplified)
    ("Heiti TC"                            :licenses (apple) :chinese traditional)
    ("Helvetica CY"                        :licenses (apple))
    ("Helvetica Neue"                      :licenses (apple))
    ("Helvetica"                           :licenses (apple))
    ("Herculanum"                          :licenses (apple))
    ("High Tower Text"                     :licenses (microsoft))
    ("Hiragino Kaku Gothic Pro"            :licenses (apple))
    ("Hiragino Kaku Gothic ProN"           :licenses (apple))
    ("Hiragino Kaku Gothic Std"            :licenses (apple))
    ("Hiragino Kaku Gothic StdN"           :licenses (apple))
    ("Hiragino Maru Gothic Pro"            :licenses (apple))
    ("Hiragino Maru Gothic ProN"           :licenses (apple))
    ("Hiragino Mincho Pro"                 :licenses (apple))
    ("Hiragino Mincho ProN"                :licenses (apple))
    ("Hiragino Sans GB"                    :licenses (apple))
    ("Hoefler Text"                        :licenses (apple))
    ("Holidays MT"                         :licenses (microsoft))
    ("Homa"                                :licenses (free) :arabic farsi)
    ("Impact"                              :licenses (apple microsoft))
    ("Imprint MT Shadow"                   :licenses (microsoft))
    ("InaiMathi"                           :licenses (apple))
    ("Informal Roman"                      :licenses (microsoft))
    ("IrisUPC"                             :licenses (microsoft))
    ("Iskoola Pota"                        :licenses (microsoft))
    ("Issa & Giliana Classic"              :licenses (free))
    ("JasmineUPC"                          :licenses (microsoft))
    ("Jokerman"                            :licenses (microsoft))
    ("Jomolhari"                           :licenses (free))
    ("Juice ITC"                           :licenses (microsoft))
    ("Junicode"                            :licenses (free))
    ("Kai"                                 :licenses (apple) :chinese simplified)
    ("KaiTi"                               :licenses (microsoft))
    ("Kailasa"                             :licenses (apple))
    ("Kaiti SC"                            :licenses (apple) :chinese simplified)
    ("Kalinga"                             :licenses (microsoft))
    ("Kannada MN"                          :licenses (apple))
    ("Kannada Sangam MN"                   :licenses (apple))
    ("Kartika"                             :licenses (microsoft))
    ("Kayases"                             :licenses (free))
    ("Kedage"                              :licenses (free))
    ("Kefa"                                :licenses (apple))
    ("Keyboard"                            :licenses (apple))
    ("Keystrokes MT"                       :licenses (microsoft))
    ("Khmer Busra"                         :licenses (free))
    ("Khmer MN"                            :licenses (apple))
    ("Khmer Mondulkiri"                    :licenses (free))
    ("Khmer Sangam MN"                     :licenses (apple))
    ("Khmer UI"                            :licenses (microsoft))
    ("KhojkiUnicodeOT"                     :licenses (free))
    ("Kino MT"                             :licenses (microsoft))
    ("Kisiska"                             :licenses (free))
    ("KodchiangUPC"                        :licenses (microsoft))
    ("Kokila"                              :licenses (microsoft))
    ("Kokonor"                             :licenses (apple))
    ("Koodak"                              :licenses (free) :arabic farsi)
    ("Kristen ITC"                         :licenses (microsoft))
    ("Krungthep"                           :licenses (apple))
    ("Kufi Extended Outline"               :licenses (microsoft) :decorative t :arabic kufic)
    ("Kufi Outline Shaded"                 :licenses (microsoft) :decorative t :arabic kufic)
    ("KufiStandardGK"                      :licenses (apple) :arabic kufic)
    ("Kunstler Script"                     :licenses (microsoft))
    ("Lanna Alif"                          :licenses (free))
    ("Lanna Unicode UI"                    :licenses (free))
    ("Lantinghei SC"                       :licenses (apple) :chinese simplified)
    ("Lantinghei TC"                       :spacing mono :licenses (apple) :chinese traditional)
    ("Lao MN"                              :licenses (apple))
    ("Lao Sangam MN"                       :licenses (apple))
    ("Lao UI"                              :licenses (microsoft))
    ("Latha"                               :licenses (microsoft))
    ("LCD"                                 :licenses (microsoft))
    ("Leelawadee"                          :licenses (microsoft))
    ("Levenim MT"                          :licenses (microsoft))
    ("LiHei Pro"                           :licenses (apple) :chinese traditional)
    ("LiSong Pro"                          :licenses (apple) :chinese traditional)
    ("Libian SC"                           :licenses (apple) :chinese simplified)
    ("LilyUPC"                             :licenses (microsoft))
    ("Lisu Tzimu"                          :licenses (free))
    ("Lisu Unicode"                        :licenses (free))
    ("Lucida Blackletter"                  :licenses (microsoft))
    ("Lucida Bright Math"                  :licenses (microsoft))
    ("Lucida Bright"                       :licenses (microsoft))
    ("Lucida Calligraphy"                  :licenses (microsoft) :decorative t)
    ("Lucida Console"                      :spacing mono :licenses (microsoft))
    ("Lucida Fax"                          :licenses (microsoft))
    ("Lucida Grande"                       :licenses (apple))
    ("Lucida Handwriting"                  :licenses (microsoft) :decorative t)
    ("Lucida Sans Typewriter"              :licenses (microsoft))
    ("Lucida Sans Unicode"                 :licenses (microsoft))
    ("Lucida Sans"                         :licenses (microsoft))
    ("Maduram"                             :licenses (free))
    ("Magneto"                             :licenses (microsoft))
    ("Maiandra GD"                         :licenses (microsoft))
    ("Malayalam MN"                        :licenses (apple))
    ("Malayalam Sangam MN"                 :licenses (apple))
    ("Malgun Gothic"                       :licenses (microsoft))
    ("Mangal"                              :licenses (microsoft))
    ("Map Symbols"                         :licenses (microsoft))
    ("Marib"                               :licenses (commercial))
    ("Marion"                              :licenses (apple))
    ("MarathiCursiveG"                     :licenses (free))
    ("Marker Felt"                         :licenses (apple) :decorative t)
    ("Marlett"                             :licenses (microsoft))
    ("Masinahikan"                         :licenses (free))
    ("Masinahikan Dene"                    :licenses (free))
    ("Masterpiece Uni Sans"                :licenses (free))
    ("Matisse ITC"                         :licenses (microsoft))
    ("Matura MT Script Capitals"           :licenses (microsoft))
    ("McZee"                               :licenses (microsoft))
    ("Mead"                                :licenses (microsoft))
    ("Meetei Mayek"                        :licenses (free))
    ("Meiryo"                              :licenses (microsoft))
    ("Meiryo UI"                           :licenses (microsoft))
    ("Menlo"                               :licenses (apple))
    ("Mercurius Script MT"                 :licenses (microsoft))
    ("Miao Unicode"                        :licenses (free))
    ("Microsoft Himalaya"                  :licenses (microsoft))
    ("Microsoft JhengHei"                  :chinese traditional :licenses (microsoft))
    ("Microsoft JhengHei UI"               :chinese traditional :licenses (microsoft))
    ("Microsoft New Tai Lue"               :licenses (microsoft))
    ("Microsoft PhagsPa"                   :licenses (microsoft))
    ("Microsoft Sans Serif"                :licenses (apple microsoft) :arabic naskh)
    ("Microsoft Tai Le"                    :licenses (microsoft))
    ("Microsoft Uighur"                    :licenses (microsoft) :arabic uighur)
    ("Microsoft YaHei"                     :chinese simplified :licenses (microsoft) :cleartype t)
    ("Microsoft YaHei UI"                  :chinese simplified :licenses (microsoft))
    ("Microsoft Yi Baiti"                  :licenses (microsoft))
    ("MingLiU"                             :spacing mono :chinese traditional :licenses (microsoft))
    ("Mingzat"                             :licenses (free))
    ("Minion Web"                          :licenses (microsoft))
    ("Miriam"                              :licenses (microsoft))
    ("Miriam Fixed"                        :spacing mono :licenses (microsoft))
    ("Mistral"                             :licenses (microsoft))
    ("Modern No. 20"                       :licenses (microsoft))
    ("Monaco"                              :spacing mono :licenses (apple))
    ("Mongolian Baiti"                     :licenses (microsoft))
    ("Monlam Uni Sans Serif"               :licenses (free))
    ("Monotype Corsiva"                    :licenses (microsoft))
    ("Monotype Sorts"                      :licenses (microsoft))
    ("MoolBoran"                           :licenses (microsoft))
    ("MPH 2B Damase"                       :licenses (free) :arabic naskh)
    ("MS Gothic"                           :spacing mono :licenses (microsoft))
    ("Mro Unicode"                         :licenses (free))
    ("MS LineDraw"                         :licenses (microsoft))
    ("MS Mincho"                           :spacing mono :licenses (microsoft))
    ("MS Outlook"                          :licenses (microsoft))
    ("MS PGothic"                          :licenses (microsoft))
    ("MS PMincho"                          :licenses (microsoft))
    ("MS Reference Sans Serif"             :licenses (microsoft))
    ("MS Reference Specialty"              :licenses (microsoft))
    ("MS Reference"                        :licenses (microsoft))
    ("MS UI Gothic"                        :licenses (microsoft))
    ("Mshtakan"                            :licenses (apple))
    ("MuktamsiddhamG"                      :licenses (free))
    ("Mukti Narrow"                        :licenses (free))
    ("Musica"                              :licenses (free))
    ("MT Extra"                            :licenses (microsoft))
    ("MV Boli"                             :licenses (microsoft))
    ("Myanmar MN"                          :licenses (apple))
    ("Myanmar Sangam MN"                   :licenses (apple))
    ("Myanmar Text"                        :licenses (microsoft))
    ("Myanmar3"                            :licenses (free))
    ("Nadeem"                              :licenses (apple) :arabic naskh)
    ("Namdhinggo SIL"                      :licenses (free))
    ("Nanum Brush Script"                  :licenses (apple))
    ("Nanum Gothic"                        :licenses (apple))
    ("Nanum Myeongjo"                      :licenses (apple))
    ("Nanum Pen Script"                    :licenses (apple))
    ("Narkisim"                            :licenses (microsoft))
    ("Nazli"                               :licenses (free) :arabic farsi)
    ("New Athena Unicode"                  :licenses (free))
    ("New Caledonia"                       :licenses (microsoft))
    ("New Peninim MT"                      :licenses (apple))
    ("News Gothic MT"                      :licenses (microsoft))
    ("Niagara"                             :licenses (microsoft))
    ("Niagara Engraved"                    :licenses (microsoft))
    ("Niagara Solid"                       :licenses (microsoft))
    ("Nilus"                               :licenses (free))
    ("Nirmala UI"                          :licenses (microsoft))
    ("Noteworthy"                          :licenses (apple))
    ("Noto Naskh Arabic"                   :licenses (free) :arabic kufic)
    ("Noto Sans"                           :licenses (free))
    ("Noto Sans Armenian"                  :licenses (free))
    ("Noto Sans Avestan"                   :licenses (free))
    ("Noto Sans Balinese"                  :licenses (free))
    ("Noto Sans Bamum"                     :licenses (free))
    ("Noto Sans Batak"                     :licenses (free))
    ("Noto Sans Bengali"                   :licenses (free))
    ("Noto Sans Bengali UI"                :licenses (free))
    ("Noto Sans Brahmi"                    :licenses (free))
    ("Noto Sans Buginese"                  :licenses (free))
    ("Noto Sans Buhid"                     :licenses (free))
    ("Noto Sans Canadian Aboriginal"       :licenses (free))
    ("Noto Sans Carian"                    :licenses (free))
    ("Noto Sans Cham"                      :licenses (free)) ; note, OS X bug?
    ("Noto Sans Cherokee"                  :licenses (free))
    ("Noto Sans Coptic"                    :licenses (free))
    ("Noto Sans Cuneiform"                 :licenses (free)) ; new name
    ("Noto Sans Cypriot"                   :licenses (free))
    ("Noto Sans Deseret"                   :licenses (free))
    ("Noto Sans Devanagari"                :licenses (free))
    ("Noto Sans Egyptian Hieroglyphs"      :licenses (free))
    ("Noto Sans Ethiopic"                  :licenses (free))
    ("Noto Sans Georgian"                  :licenses (free))
    ("Noto Sans Glagolitic"                :licenses (free))
    ("Noto Sans Gothic"                    :licenses (free))
    ("Noto Sans Gujarati"                  :licenses (free))
    ("Noto Sans Gujarati UI"               :licenses (free))
    ("Noto Sans Gurmukhi"                  :licenses (free))
    ("Noto Sans Gurmukhi UI"               :licenses (free))
    ("Noto Sans Hanunoo"                   :licenses (free))
    ("Noto Sans Hebrew"                    :licenses (free))
    ("Noto Sans Imperial Aramaic"          :licenses (free))
    ("Noto Sans Inscriptional Pahlavi"     :licenses (free))
    ("Noto Sans Inscriptional Parthian"    :licenses (free))
    ("Noto Sans Japanese"                  :chinese kanji :licenses (free))
    ("Noto Sans Javanese"                  :licenses (free))
    ("Noto Sans Kaithi"                    :licenses (free))
    ("Noto Sans Kannada"                   :licenses (free))
    ("Noto Sans Kannada UI"                :licenses (free))
    ("Noto Sans Kayah Li"                  :licenses (free))
    ("Noto Sans Kharoshthi"                :licenses (free))
    ("Noto Sans Khmer"                     :licenses (free))
    ("Noto Sans Khmer UI"                  :licenses (free))
    ("Noto Sans Korean"                    :chinese hanja :licenses (free))
    ("Noto Sans Kufi Arabic"               :licenses (free) :arabic kufic) ; old name
    ("Noto Kufi Arabic"                    :licenses (free) :arabic kufic) ; new name
    ("Noto Sans Lao"                       :licenses (free))
    ("Noto Sans Lao UI"                    :licenses (free))
    ("Noto Sans Lepcha"                    :licenses (free))
    ("Noto Sans Limbu"                     :licenses (free))
    ("Noto Sans Linear B"                  :licenses (free))
    ("Noto Sans Lisu"                      :licenses (free))
    ("Noto Sans Lycian"                    :licenses (free))
    ("Noto Sans Lydian"                    :licenses (free))
    ("Noto Sans Malayalam"                 :licenses (free))
    ("Noto Sans Malayalam UI"              :licenses (free))
    ("Noto Sans Mandaic"                   :licenses (free))
    ("Noto Sans Meetei Mayek"              :licenses (free))
    ("Noto Sans Mongolian"                 :licenses (free))
    ("Noto Sans Myanmar"                   :licenses (free))
    ("Noto Sans Myanmar UI"                :licenses (free))
    ("Noto Sans NKo"                       :licenses (free))
    ("Noto Sans New Tai Lue"               :licenses (free))
    ("Noto Sans Ogham"                     :licenses (free))
    ("Noto Sans Ol Chiki"                  :licenses (free))
    ("Noto Sans Old Italic"                :licenses (free)) ; note, Emacs bug?
    ("Noto Sans Old Persian"               :licenses (free))
    ("Noto Sans Old South Arabian"         :licenses (free))
    ("Noto Sans Old Turkic"                :licenses (free))
    ("Noto Sans Oriya"                     :licenses (free))
    ("Noto Sans Osmanya"                   :licenses (free))
    ("Noto Sans Phags-pa"                  :licenses (free))
    ("Noto Sans Phoenician"                :licenses (free))
    ("Noto Sans Rejang"                    :licenses (free))
    ("Noto Sans Runic"                     :licenses (free))
    ("Noto Sans S Chinese"                 :chinese simplified :licenses (free))
    ("Noto Sans Samaritan"                 :licenses (free))
    ("Noto Sans Saurashtra"                :licenses (free))
    ("Noto Sans Shavian"                   :licenses (free))
    ("Noto Sans Sinhala"                   :licenses (free))
    ("Noto Sans Sumero-Akkadian Cuneiform" :licenses (free)) ; old name
    ("Noto Sans Sundanese"                 :licenses (free))
    ("Noto Sans Syloti Nagri"              :licenses (free))
    ("Noto Sans Symbols"                   :licenses (free))
    ("Noto Sans Syriac Eastern"            :licenses (free))
    ("Noto Sans Syriac Estrangela"         :licenses (free))
    ("Noto Sans Syriac Western"            :licenses (free))
    ("Noto Sans T Chinese"                 :chinese traditional :licenses (free))
    ("Noto Sans Tagalog"                   :licenses (free))
    ("Noto Sans Tagbanwa"                  :licenses (free))
    ("Noto Sans Tai Le"                    :licenses (free))
    ("Noto Sans Tai Tham"                  :licenses (free))
    ("Noto Sans Tai Viet"                  :licenses (free))
    ("Noto Sans Tamil"                     :licenses (free))
    ("Noto Sans Tamil UI"                  :licenses (free))
    ("Noto Sans Telugu"                    :licenses (free))
    ("Noto Sans Telugu UI"                 :licenses (free))
    ("Noto Sans Thaana"                    :licenses (free))
    ("Noto Sans Thai"                      :licenses (free))
    ("Noto Sans Thai UI"                   :licenses (free))
    ("Noto Sans Tibetan"                   :licenses (free))
    ("Noto Sans Tifinagh"                  :licenses (free))
    ("Noto Sans Ugaritic"                  :licenses (free))
    ("Noto Sans Vai"                       :licenses (free))
    ("Noto Sans Yi"                        :licenses (free))
    ("Noto Serif Armenian"                 :licenses (free))
    ("Noto Serif Georgian"                 :licenses (free))
    ("Noto Serif Khmer"                    :licenses (free))
    ("Noto Serif Lao"                      :licenses (free))
    ("Noto Serif Thai"                     :licenses (free))
    ("Noto Serif"                          :licenses (free))
    ("Nuosu SIL"                           :licenses (free))
    ("NSimSun"                             :licenses (microsoft))
    ("Nyala"                               :licenses (microsoft))
    ("OCR A Extended"                      :licenses (microsoft))
    ("OCR-B-Digits"                        :licenses (microsoft))
    ("OCRB"                                :licenses (microsoft))
    ("Old Antic Bold"                      :licenses (microsoft) :decorative t :arabic naskh)
    ("Old English Text MT"                 :licenses (microsoft))
    ("OldHungarian"                        :licenses (free))
    ("OldSindhi"                           :licenses (free))
    ("Onyx"                                :licenses (microsoft))
    ("Optima"                              :licenses (apple))
    ("Oriya MN"                            :licenses (apple))
    ("Oriya Sangam MN"                     :licenses (apple))
    ("Osaka"                               :spacing mono :licenses (apple))
    ("OskiBlackfoot"                       :licenses (free))
    ("OskiDakelh"                          :licenses (free))
    ("OskiDeneA"                           :licenses (free))
    ("OskiDeneB"                           :licenses (free))
    ("OskiDeneC"                           :licenses (free))
    ("OskiDeneS"                           :licenses (free))
    ("OskiEast"                            :licenses (free))
    ("OskiWest"                            :licenses (free))
    ("Padauk"                              :licenses (free))
    ("Palace Script MT"                    :licenses (microsoft))
    ("Palatino Linotype"                   :licenses (microsoft))
    ("Palatino"                            :licenses (apple))
    ("Papyrus"                             :licenses (apple microsoft))
    ("Parade"                              :licenses (microsoft))
    ("Parchment"                           :licenses (microsoft))
    ("Parties MT"                          :licenses (microsoft))
    ("PCMyungjo"                           :spacing mono :licenses (apple))
    ("Peignot Medium"                      :licenses (microsoft))
    ("Pepita MT"                           :licenses (microsoft))
    ("Penuturesu"                          :licenses (free))
    ("Perpetua Titling MT"                 :licenses (microsoft))
    ("Perpetua"                            :licenses (microsoft))
    ("PilGi"                               :licenses (apple) :glyph-quality low)
    ("Pitabek"                             :licenses (free))
    ("Placard Condensed"                   :licenses (microsoft))
    ("Plantagenet Cherokee"                :licenses (apple microsoft))
    ("Playbill"                            :licenses (microsoft))
    ("PMingLiU"                            :licenses (microsoft))
    ("Poor Richard"                        :licenses (microsoft))
    ("Pristina"                            :licenses (microsoft))
    ("PT Sans"                             :licenses (apple))
    ("Qataban"                             :licenses (free))
    ("Quivira"                             :licenses (free))
    ("Raanana"                             :licenses (apple))
    ("Raavi"                               :licenses (microsoft))
    ("Rage"                                :licenses (microsoft))
    ("Ransom"                              :licenses (microsoft))
    ("Ravie"                               :licenses (microsoft))
    ("RefSpecialty"                        :licenses (microsoft))
    ("Ribeng"                              :licenses (free))
    ("Rockwell"                            :licenses (microsoft))
    ("Rockwell Condensed"                  :licenses (microsoft))
    ("Rod"                                 :licenses (microsoft))
    ("Rotinonhsonni Sans"                  :licenses (free))
    ("Rotinonhsonni Serif"                 :licenses (free))
    ("Roya"                                :licenses (free) :arabic farsi)
    ("Runic MT Condensed"                  :licenses (microsoft))
    ("Sakkal Majalla"                      :licenses (microsoft))
    ("Sadagolthina"                        :licenses (commercial))
    ("Samyak"                              :licenses (free))
    ("Samyak Devanagari"                   :licenses (free))
    ("Samyak Gujarati"                     :licenses (free))
    ("Samyak Malayalam"                    :licenses (free))
    ("Samyak Oriya"                        :licenses (free))
    ("Samyak Tamil"                        :licenses (free))
    ("Sathu"                               :licenses (apple))
    ("Saysettha MX"                        :licenses (free))
    ("Scheherazade"                        :licenses (free))
    ("Script MT"                           :licenses (microsoft))
    ("Segoe Chess"                         :licenses (microsoft))
    ("Segoe Print"                         :licenses (microsoft))
    ("Segoe Script"                        :licenses (microsoft))
    ("Segoe UI"                            :licenses (microsoft) :cleartype t)
    ("Segoe UI Historic"                   :licenses (microsoft) :cleartype t)
    ("Segoe UI Symbol"                     :licenses (microsoft) :cleartype t)
    ("Shonar Bangla"                       :licenses (microsoft))
    ("Showcard Gothic"                     :licenses (microsoft))
    ("Shruti"                              :licenses (microsoft))
    ("Siddhanta"                           :licenses (free))
    ("Signs MT"                            :licenses (microsoft))
    ("Silom"                               :licenses (apple))
    ("SimHei"                              :spacing mono :chinese simplified :licenses (microsoft))
    ("SimSun"                              :spacing mono :chinese simplified :licenses (microsoft apple))
    ("Simplified Arabic"                   :spacing mono :licenses (microsoft) :arabic naskh)
    ("Simplified Arabic Fixed"             :spacing mono :licenses (microsoft) :arabic naskh)
    ("Sinhala MN"                          :licenses (apple))
    ("Sinhala Sangam MN"                   :licenses (apple))
    ("Skia"                                :licenses (apple))
    ("Snap ITC"                            :licenses (microsoft))
    ("Songti SC"                           :licenses (apple))
    ("Sourashtra"                          :licenses (free))
    ("Sports MT"                           :licenses (microsoft))
    ("STFangsong"                          :licenses (apple) :chinese simplified)
    ("STHeiti"                             :licenses (apple) :chinese simplified)
    ("STKaiti"                             :licenses (apple) :chinese simplified)
    ("STSong"                              :licenses (apple) :chinese simplified)
    ("Stencil"                             :licenses (microsoft))
    ("STIXGeneral"                         :licenses (apple free))
    ("STIXIntegralsD"                      :licenses (apple free))
    ("STIXIntegralsSm"                     :licenses (apple free))
    ("STIXIntegralsUpD"                    :licenses (apple free))
    ("STIXIntegralsUpSm"                   :licenses (apple free))
    ("STIXIntegralsUp"                     :licenses (apple free))
    ("STIXNonUnicode"                      :licenses (apple free))
    ("STIXSizeFiveSym"                     :licenses (apple free))
    ("STIXSizeFourSym"                     :licenses (apple free))
    ("STIXSizeOneSym"                      :licenses (apple free))
    ("STIXSizeThreeSym"                    :licenses (apple free))
    ("STIXSizeTwoSym"                      :licenses (apple free))
    ("STIXVariants"                        :licenses (apple free))
    ("Stop"                                :licenses (microsoft))
    ("Sundanese Unicode"                   :licenses (free))
    ("Sylfaen"                             :licenses (microsoft))
    ("Symbol"                              :licenses (apple microsoft))
    ("Symbola"                             :licenses (free))
    ("Tahoma"                              :licenses (apple microsoft) :arabic naskh)
    ("Tai Heritage Pro"                    :licenses (free))
    ("Tamil MN"                            :licenses (apple))
    ("Tamil Sangam MN"                     :licenses (apple))
    ("Telugu MN"                           :licenses (apple))
    ("Telugu Sangam MN"                    :licenses (apple))
    ("Tempo Grunge"                        :licenses (microsoft))
    ("Tempus Sans ITC"                     :licenses (microsoft))
    ("Terafik"                             :licenses (free) :arabic farsi)
    ("TharLon"                             :licenses (free))
    ("Thonburi"                            :licenses (apple))
    ("Tibetan Machine Uni"                 :licenses (free))
    ("Times"                               :licenses (apple))
    ("Times New Roman"                     :licenses (apple microsoft))
    ("Times New Roman Special"             :licenses (microsoft))
    ("Tinos"                               :licenses (free))
    ("Titr"                                :licenses (free) :arabic farsi)
    ("Traditional Arabic"                  :licenses (microsoft) :arabic naskh)
    ("Transport MT"                        :licenses (microsoft))
    ("Trebuchet MS"                        :licenses (apple microsoft))
    ("Tuladha Jejeg"                       :licenses (free))
    ("Tunga"                               :licenses (microsoft))
    ("Tw Cen MT"                           :licenses (microsoft))
    ("Tw Cen MT Condensed"                 :licenses (microsoft))
    ("UnBatang"                            :chinese hanja :licenses (free))
    ("unifont"                             :licenses (free) :glyph-quality low)
    ("Utsaah"                              :licenses (microsoft))
    ("Urdu Typesetting"                    :licenses (microsoft) :arabic urdu)
    ("Vacation MT"                         :licenses (microsoft))
    ("Vani"                                :licenses (microsoft))
    ("Verdana Ref"                         :licenses (microsoft))
    ("Verdana"                             :licenses (apple microsoft))
    ("Vijaya"                              :licenses (microsoft))
    ("Viner Hand ITC"                      :licenses (microsoft))
    ("Vivaldi"                             :licenses (microsoft))
    ("Tai Le Valentinium"                  :licenses (free))
    ("Vixar ASCI"                          :licenses (microsoft))
    ("Vladimir Script"                     :licenses (microsoft))
    ("Vrinda"                              :licenses (microsoft))
    ("Wakor"                               :licenses (free))
    ("Wawati SC"                           :chinese simplified :licenses (apple))
    ("Wawati TC"                           :chinese traditional :licenses (apple))
    ("Webdings"                            :licenses (apple microsoft))
    ("Weibei SC"                           :chinese simplified :licenses (apple))
    ("Weibei TC"                           :chinese traditional :licenses (apple))
    ("WenQuanYi Zen Hei"                   :licenses (free) :chinese simplified)
    ("WenQuanYi Zen Hei Mono"              :spacing mono :licenses (free) :chinese simplified)
    ("Westminster"                         :licenses (microsoft))
    ("Wide Latin"                          :licenses (microsoft))
    ("Wingdings"                           :licenses (apple microsoft))
    ("Wingdings 2"                         :licenses (apple microsoft))
    ("Wingdings 3"                         :licenses (apple microsoft))
    ("Xingkai SC"                          :chinese simplified :licenses (apple) :decorative t)
    ("Yuanti SC"                           :chinese simplified :licenses (apple))
    ("Yunghkio"                            :licenses (free))
    ("Yuppy SC"                            :chinese simplified :licenses (apple))
    ("Yuppy TC"                            :chinese traditional :licenses (apple))
    ("Zapf Dingbats"                       :licenses (apple))
    ("Zapfino"                             :licenses (apple))
    ("ZH Mono"                             :licenses (free))
    ))

;;; customizable variables

;;;###autoload
(defgroup unicode-fonts nil
  "Configure Unicode fonts."
  :version "0.4.10"
  :link '(emacs-commentary-link :tag "Commentary" "unicode-fonts")
  :link '(url-link :tag "GitHub" "http://github.com/rolandwalker/unicode-fonts")
  :link '(url-link :tag "EmacsWiki" "http://emacswiki.org/emacs/UnicodeFonts")
  :prefix "unicode-fonts-"
  :group 'i18n
  :group 'faces)

;;;###autoload
(defgroup unicode-fonts-tweaks nil
  "Tweaks for `unicode-fonts', especially regarding font availability."
  :group 'unicode-fonts)

(defcustom unicode-fonts-use-prepend (not (or (eq window-system 'ns)
                                              (eq window-system 'mac)))
  "Whether the 'prepend argument to `set-fontset-font' works.

Whether this argument works is dependent on your operating system
and the font backend used by your Emacs build.

This defaults to nil when using the Cocoa or native Mac font
backends on OS X, t otherwise."
  :type 'boolean
  :group 'unicode-fonts-tweaks)

(defcustom unicode-fonts-existence-checks 'all
  "How unicode-fonts will dynamically check fonts at startup.

This option dramatically affects startup time, but is not
recommended to change from the default.

\"Check All Fonts at Startup\" is the slowest, but provides
full sanity-checking and the maximum number of gplyhs made
displayable.

\"Only First Existing Font\" is five to ten times faster than
checking all fonts.  The drawback is that fewer fallbacks will
be provided, meaning that fewer glyphs may be displayable.

\"Load All Fonts Without Checking\" is fast and provides the
maximum number of fallbacks, but Emacs could behave unpredictably
when it is instructed to display using a nonexistent font."
  :type '(choice
          (const :tag "Check All Fonts at Startup"               all)
          (const :tag "Only First Existing Font for Each Block"  first)
          (const :tag "Load All Fonts Without Checking"          none))
  :group 'unicode-fonts-tweaks)

(defcustom unicode-fonts-restrict-to-fonts nil
  "Limit fonts (and font-existence checks) to only those listed here.

This is a way to speed startup by informing Emacs ahead of
time that only certain fonts are present.

Each font name is a string, typically in Fontconfig font-name
format.

Leave the list empty for no restriction."
  :type '(repeat string)
  :group 'unicode-fonts-tweaks)

(defcustom unicode-fonts-skip-fonts nil
  "Skip over the fonts listed here.  Do not apply them as defaults.

This can be used to speed startup time, and also to enforce
choices of style.

Note, however, that this package merely provides clues to Emacs
about which fonts are good.  Even if this package skips over a
font, Emacs may still choose that font if you don't provide
a better clue.

Each font name is a string, typically in Fontconfig font-name
format.

Leave the list empty for no per-font exclusions."
  :type '(repeat string)
  :group 'unicode-fonts-tweaks)

(defcustom unicode-fonts-skip-font-groups (cond
                                            ((eq window-system 'w32)
                                             '(buggy-before-vista decorative low-quality-glyphs multicolor))
                                            ((eq window-system 'mac)
                                             '(decorative low-quality-glyphs))
                                            (t
                                             '(decorative low-quality-glyphs multicolor)))
  "Skip over groups of fonts listed here.

This can be used to speed startup time, and also to enforce
choices of style.

Note well: each set is defined from a list of properties kept
within this library.  This listing is perforce incomplete,
therefore this setting cannot be expected to work very well
with regard to enforcing style.

It may help you get started.

Leave the list empty for no per-group exclusions."
  :type '(set  (const :tag "Simplified Chinese Script"           chinese-simplified)
               (const :tag "Traditional Chinese Script"          chinese-traditional)
               (const :tag "Hanja Chinese Script"                chinese-hanja)
               (const :tag "Kanji Chinese Script"                chinese-kanji)
               (const :tag "Nôm Chinese Script"                  chinese-nom)
               (const :tag "Naskh Arabic Script"                 arabic-naskh)
               (const :tag "Diwani Arabic Script"                arabic-diwani)
               (const :tag "Farsi Arabic Script (Nastaleeq)"     arabic-farsi)
               (const :tag "Urdu Arabic Script (Nastaleeq)"      arabic-urdu)
               (const :tag "Kufic Arabic Script"                 arabic-kufic)
               (const :tag "Available only from Microsoft"       microsoft-only)
               (const :tag "Available from Microsoft and others" microsoft)
               (const :tag "Non-ClearType"                       non-cleartype)
               (const :tag "Available only from Apple"           apple-only)
               (const :tag "Available from Apple and others"     apple)
               (const :tag "Multicolor Glyphs"                   multicolor)
               (const :tag "Free"                                free)
               (const :tag "Non-free"                            non-free)
               (const :tag "Decorative"                          decorative)
               (const :tag "Low Quality Glyphs"                  low-quality-glyphs)
               (const :tag "Buggy Display Before Vista"          buggy-before-vista))
  :group 'unicode-fonts-tweaks)

;;;###autoload
(defgroup unicode-fonts-debug nil
  "Settings for debugging `unicode-fonts'."
  :group 'unicode-fonts)

(defcustom unicode-fonts-debug-availability nil
  "Debug font availability to the messages buffer."
  :type 'boolean
  :group 'unicode-fonts-debug)

;;; toplevel customize group

(defcustom unicode-fonts-use-persistent-storage "unicode-fonts"
  "Use persistent disk storage when available.

This greatly reduces startup time.

Internally, this value is a string which is used for the filename
of the persistent data store."
  :type '(choice
          (const :tag "Yes"  "unicode-fonts")
          (const :tag "No"   nil))
  :group 'unicode-fonts)

(defcustom unicode-fonts-less-feedback nil
  "Give less echo area feedback.

Leaving this off allows you to see the impact of this
library on startup time."
  :type 'boolean
  :group 'unicode-fonts)

(defcustom unicode-fonts-fallback-font-list '("Symbola" "Quivira")
  "Candidates for a general fallback font.

The fonts from this list will be used, in order, for characters
which have no explicit mapping.

Each font name is a string, typically in Fontconfig font-name
format.

Set to nil to disable."
  :type '(repeat string)
  :group 'unicode-fonts)

(defcustom unicode-fonts-fontset-names '("fontset-default" "fontset-startup" "fontset-standard")
  "Fontsets in which to install mappings via `set-fontset-font'."
  :type '(repeat string)
  :group 'unicode-fonts)

(defcustom unicode-fonts-block-font-mapping
  '(("Aegean Numbers"                                   (
                                                         "Noto Sans Symbols"            ; 57/57
                                                         "Aegean"                       ; 57/57
                                                         "Symbola"                      ; 57/57  @8.00 @8
                                                         "Quivira"                      ; 57/57  @4.1 @3.80000305175781
                                                         "Code2001"                     ; 57/57
                                                         "Everson Mono:weight=bold"     ; 57/57
                                                         "ALPHABETUM Unicode"           ; 57/57
                                                         ))
    ("Ahom"                                             (
                                                         "AhomUnicode"                  ; 57/57
                                                         ))
    ("Alchemical Symbols"                               (
                                                         "Noto Sans Symbols"            ; 116/116
                                                         "Symbola"                      ; 116/116  @8.00 @8
                                                         "Quivira"                      ; 116/116  @4.1 @3.80000305175781
                                                         "Everson Mono:weight=bold"     ; 116/116
                                                         ))
    ("Alphabetic Presentation Forms"                    (
                                                         "DejaVu Sans:width=condensed"  ; 58/58
                                                         "Arial Unicode MS"             ; 57/58
                                                         "Cardo"                        ; 58/58
                                                         "Code2000"
                                                         "Quivira"                      ; 58/58  @4.1 @3.80000305175781
                                                         "Everson Mono:weight=bold"     ; 58/58
                                                         "FreeMono"                     ; 52/58
                                                         "ALPHABETUM Unicode"           ; 53/58
                                                         ))
    ("Anatolian Hieroglyphs"                            (
                                                         "Anatolian"                    ; 583/583
                                                         ))
    ("Ancient Greek Musical Notation"                   (
                                                         "Cardo"                        ; 70/70
                                                         "Noto Sans Symbols"            ; 70/70
                                                         "Aegean"                       ; 70/70
                                                         "New Athena Unicode"           ; 70/70
                                                         "Musica"                       ; 70/70
                                                         "Symbola"                      ; 70/70  @8.00 @8
                                                         "Quivira"                      ; 70/70  @4.1 @3.80000305175781
                                                         "Everson Mono:weight=bold"     ; 70/70
                                                         "ALPHABETUM Unicode"           ; 70/70
                                                         ))
    ("Ancient Greek Numbers"                            (
                                                         "Noto Sans Symbols"            ; 75/77
                                                         "Apple Symbols"                ; 66/77
                                                         "New Athena Unicode"           ; 75/77
                                                         "Cardo"                        ; 75/77
                                                         "Aegean"                       ; 75/77
                                                         "Quivira"                      ; 77/77  @4.1 @3.80000305175781
                                                         "Symbola"                      ; 77/77  @8.00 @8
                                                         "Everson Mono:weight=bold"     ; 75/77
                                                         "ALPHABETUM Unicode"           ; 75/77
                                                         ))
    ("Ancient Symbols"                                  (
                                                         "Noto Sans Symbols"            ; 12/13
                                                         "Analecta"                     ; 12/13
                                                         "New Athena Unicode"           ; 12/13
                                                         "Cardo"                        ; 12/13
                                                         "Aegean"                       ; 13/13
                                                         "Quivira"                      ; 13/13  @4.1 @3.80000305175781
                                                         "Symbola"                      ; 13/13  @8.00 @8
                                                         "Everson Mono:weight=bold"     ; 12/13
                                                         "ALPHABETUM Unicode"           ; 12/13
                                                         ))
    ("Arabic"                                           (
                                                         "Courier New"
                                                         "Simplified Arabic Fixed"
                                                         "Simplified Arabic"
                                                         "Amiri"                        ; 252/255
                                                         "Aldhabi"
                                                         "Adobe Arabic"                 ; 209/255
                                                         "Urdu Typesetting"
                                                         "Geeza Pro"
                                                         "Baghdad"                      ;  81/255
                                                         "Damascus"
                                                         "Al Bayan"                     ;  79/255
                                                         "Andalus"
                                                         "Arabic Typesetting"
                                                         "Traditional Arabic"
                                                         "Scheherazade"                 ; 255/255
                                                         "DejaVu Sans Mono"             ;  99/255  @2.35 @2.34999084472656
                                                         "DejaVu Sans:width=condensed"
                                                         "Arial Unicode MS"
                                                         "Nadeem"
                                                         "Microsoft Uighur"
                                                         "Tahoma"
                                                         "Microsoft Sans Serif"
                                                         "MPH 2B Damase"
                                                         "KufiStandardGK"
                                                         "DecoType Naskh"
                                                         "Koodak"
                                                         "FreeMono"                     ; 212/255
                                                         "Code2000"
                                                         ))
    ("Arabic Extended-A"                                (
                                                         "Scheherazade"                 ; 47/50
                                                         "Amiri"                        ;  4/50
                                                         ))
    ("Arabic Mathematical Alphabetic Symbols"           (
                                                         "Amiri"                        ; 141/143
                                                         ))
    ("Arabic Presentation Forms-A"                      (
                                                         "Geeza Pro"                    ; 595/611
                                                         "Amiri"                        ; 610/611
                                                         "Arial Unicode MS"             ; 593/611
                                                         "Microsoft Sans Serif"
                                                         "Tahoma"
                                                         "KufiStandardGK"
                                                         "Andalus"
                                                         "Arabic Typesetting"
                                                         "Urdu Typesetting"
                                                         "Adobe Arabic"                 ; 171/611
                                                         "DecoType Naskh"               ;  57/611
                                                         "Al Bayan"                     ;  62/611
                                                         "DejaVu Sans Mono"             ;  72/611  @2.35 @2.34999084472656
                                                         "DejaVu Sans:width=condensed"  ;  98/611
                                                         "MPH 2B Damase"                ;  24/611
                                                         "Code2000"                     ; 155/611
                                                         ))
    ("Arabic Presentation Forms-B"                      (
                                                         "DejaVu Sans Mono"             ; 141/141  @2.35 @2.34999084472656
                                                         "Geeza Pro"                    ; 140/141
                                                         "Amiri"                        ; 139/141
                                                         "Adobe Arabic"                 ; 125/141
                                                         "Traditional Arabic"
                                                         "Urdu Typesetting"
                                                         "Arial Unicode MS"
                                                         "Microsoft Sans Serif"
                                                         "KufiStandardGK"
                                                         "DejaVu Sans:width=condensed"  ; 140/141
                                                         "FreeMono"                     ; 141/141
                                                         "DecoType Naskh"               ;  89/141
                                                         "Code2000"
                                                         ))
    ("Arabic Supplement"                                (
                                                         "Courier New"
                                                         "Simplified Arabic Fixed"
                                                         "Amiri"                        ; 48/48
                                                         "Simplified Arabic"
                                                         "Geeza Pro"
                                                         "Damascus"
                                                         "Andalus"
                                                         "Arabic Typesetting"
                                                         "Traditional Arabic"
                                                         "Scheherazade"                 ; 48/48
                                                         "Adobe Arabic"                 ; 30/48
                                                         "Microsoft Uighur"
                                                         "Tahoma"
                                                         "Microsoft Sans Serif"
                                                         "MPH 2B Damase"
                                                         ))
    ("Armenian"                                         (
                                                         "DejaVu Sans Mono"             ; 86/89  @2.35 @2.34999084472656
                                                         "Noto Sans Armenian"           ; 87/89
                                                         "Mshtakan"
                                                         "Sylfaen"
                                                         "DejaVu Sans:width=condensed"
                                                         "Quivira"                      ; 89/89  @4.1 @3.80000305175781
                                                         "MPH 2B Damase"
                                                         "Code2000"
                                                         "Arial Unicode MS"
                                                         "Everson Mono:weight=bold"     ; 89/89
                                                         "FreeMono"                     ; 87/89
                                                         ))
    ("Arrows"                                           (
                                                         "DejaVu Sans Mono"             ; 112/112  @2.35 @2.34999084472656
                                                         "Apple Symbols"                ; 112/112
                                                         "Cambria Math"                 ; 112/112
                                                         "Segoe UI Symbol"              ; 112/112
                                                         "DejaVu Sans:width=condensed"
                                                         "Asana Math"                   ; 112/112
                                                         "Arial Unicode MS"
                                                         "BabelStone Modern"            ; 102/112
                                                         "Symbola"                      ; 112/112  @8.00 @8
                                                         "Quivira"                      ; 112/112  @4.1 @3.80000305175781
                                                         "Code2000"
                                                         "Noto Sans Symbols"            ; 112/112
                                                         "Everson Mono:weight=bold"     ; 112/112
                                                         "FreeMono"                     ;  77/112
                                                         ))
    ("Avestan"                                          (
                                                         "Noto Sans Avestan"            ; 61/61
                                                         "Ahuramzda:weight=bold"        ; 61/61
                                                         "ALPHABETUM Unicode"           ; 61/61
                                                         ))
    ("Balinese"                                         (
                                                         "Noto Sans Balinese:weight=bold" ; 121/121
                                                         "Aksara Bali"                    ; 121/121
                                                         ))
    ("Bamum"                                            (
                                                         "Noto Sans Bamum"              ; 88/88
                                                         ))
    ("Bamum Supplement"                                 (
                                                         "Noto Sans Bamum"              ; 569/569
                                                         ))
    ;; ("Basic Latin"                                   (""))                           ; covered by the default font
    ;; ("Bassa Vah"                                     (""))                           ; todo added in Unicode 7.0
    ("Batak"                                            (
                                                         "Batak-Unicode"                ; 56/56
                                                         "Noto Sans Batak"              ; 56/56
                                                         ))
    ("Bengali"                                          (
                                                         "Bangla Sangam MN"
                                                         "Noto Sans Bengali"            ; 92/93
                                                         "Noto Sans Bengali UI"         ; 92/93
                                                         "Nirmala UI"
                                                         "Vrinda"
                                                         "Mukti Narrow"
                                                         "Akaash"
                                                         "Arial Unicode MS"
                                                         "Code2000"
                                                         "ALPHABETUM Unicode"           ; 87/93
                                                         ))
    ("Block Elements"                                   (
                                                         "DejaVu Sans Mono"             ; 32/32  @2.35 @2.34999084472656
                                                         "Noto Sans Symbols"            ; 32/32
                                                         "FreeMono"                     ; 32/32
                                                         "DejaVu Sans:width=condensed"
                                                         "Apple Symbols"                ; 32/32
                                                         "Segoe UI Symbol"              ; 32/32
                                                         "BabelStone Modern"            ; 32/32
                                                         "Symbola"                      ; 32/32  @8.00 @8
                                                         "Quivira"                      ; 32/32  @4.1 @3.80000305175781
                                                         "Code2000"
                                                         "Everson Mono:weight=bold"     ; 32/32
                                                         ))
    ("Bopomofo"                                         (                               ; prefer traditional
                                                         "Lantinghei TC"
                                                         "MingLiU"
                                                         "SimHei"
                                                         "LiSong Pro"                   ; 37/41
                                                         "FangSong"
                                                         "SimSun"
                                                         "DFKai-SB"
                                                         "WenQuanYi Zen Hei Mono"       ; 41/41
                                                         "Microsoft JhengHei"
                                                         "Microsoft JhengHei UI"
                                                         "Microsoft YaHei"
                                                         "Microsoft YaHei UI"
                                                         "Lantinghei SC"
                                                         "HAN NOM A"                    ; 37/41
                                                         "Arial Unicode MS"
                                                         "BabelStone Han"               ; 41/41
                                                         "Code2000"
                                                         "ALPHABETUM Unicode"           ; 41/41
                                                         ))
    ("Bopomofo Extended"                                (
                                                         "MingLiU"
                                                         "SimHei"
                                                         "FangSong"
                                                         "SimSun"
                                                         "DFKai-SB"
                                                         "Microsoft JhengHei"
                                                         "Microsoft JhengHei UI"
                                                         "Microsoft YaHei"              ; 24/27
                                                         "Microsoft YaHei UI"           ; 24/27
                                                         "BabelStone Han"               ; 27/27
                                                         "Code2000"
                                                         ))
    ("Box Drawing"                                      (
                                                         "DejaVu Sans Mono"             ; 128/128  @2.35 @2.34999084472656
                                                         "FreeMono"                     ; 128/128
                                                         "DejaVu Sans"
                                                         "Everson Mono"                 ; 128/128
                                                         "Quivira"                      ; 128/128  @4.1 @3.80000305175781
                                                         "Code2000"
                                                         "Noto Sans Symbols"            ; 128/128
                                                         "Segoe UI Symbol"              ; 128/128
                                                         "Symbola"                      ; 128/128  @8.00 @8
                                                         ))
    ("Brahmi"                                           (
                                                         "Segoe UI Historic"            ; 109/109  @1.00 @1
                                                         "Noto Sans Brahmi"             ; 108/109
                                                         "Adinatha Tamil Brahmi"        ;  45/109
                                                         "ALPHABETUM Unicode"           ;  85/109
                                                         ))
    ("Braille Patterns"                                 (
                                                         "Quivira"                      ; 256/256  @4.1 @3.80000305175781
                                                         "Apple Braille"                ; 256/256
                                                         "DejaVu Sans:width=condensed"
                                                         "Apple Symbols"                ; 256/256
                                                         "Segoe UI Symbol"              ; 256/256
                                                         "Symbola"                      ; 256/256  @8.00 @8
                                                         "Noto Sans Symbols"            ; 256/256
                                                         "FreeMono"                     ; 256/256
                                                         "Code2000"
                                                         "Everson Mono:weight=bold"     ; 256/256
                                                         ))
    ("Buginese"                                         (
                                                         "Noto Sans Buginese"           ; 30/30
                                                         "MPH 2B Damase"                ; 30/30
                                                         "Monlam Uni Sans Serif"        ; 30/30  @3.0 2008 @1.42298889160156
                                                         "Code2000"                     ; 30/30
                                                         ))
    ("Buhid"                                            (
                                                         "Noto Sans Buhid"              ; 20/20
                                                         "Quivira"                      ; 20/20  @4.1 @3.80000305175781
                                                         "Code2000"                     ; 20/20
                                                         ))
    ("Byzantine Musical Symbols"                        (
                                                         "Noto Sans Symbols"            ; 246/246
                                                         "Musica"
                                                         "Symbola"                      ; 246/246  @8.00 @8
                                                         "FreeSerif"                    ; 246/246
                                                         ))
    ("Carian"                                           (
                                                         "Segoe UI Historic"            ; 49/49  @1.00 @1
                                                         "Noto Sans Carian"             ; 49/49
                                                         "Aegean"                       ; 49/49
                                                         "Quivira"                      ; 49/49  @4.1 @3.80000305175781
                                                         "Everson Mono:weight=bold"     ; 49/49
                                                         "ALPHABETUM Unicode"           ; 49/49
                                                         ))
    ;; ("Caucasian Albanian"                            (""))                           ; todo added in Unicode 7.0
    ("Chakma"                                           (
                                                         "Ribeng"                       ; 67/67
                                                         ))
    ("Cham"                                             (
                                                         "Noto Sans Cham"               ; 83/83  @1.00_December_21_2012_initial_release @1 - bug: does not appear under this name on OS X
                                                         "Cham OI_Tangin"               ; 83/83  @1.00_September_23_2013 @1
                                                         "Cham OI_Kulbleng"             ; 83/83  @1.00_September_23_2013 @1
                                                         "Cham OI_Kul"                  ; 83/83  @1.00_September_25_2013 @1
                                                         "Code2000"                     ; 83/83
                                                         ))
    ("Cherokee"                                         (
                                                         "Aboriginal Sans"              ; 85/92
                                                         "Aboriginal Serif"             ; 85/92
                                                         "Plantagenet Cherokee"
                                                         "Noto Sans Cherokee"           ; 85/92
                                                         "Gadugi"
                                                         "MPH 2B Damase"
                                                         "Quivira"                      ; 85/92  @4.1 @3.80000305175781
                                                         "Everson Mono:weight=bold"     ; 85/92
                                                         "FreeMono"                     ; 85/92
                                                         "Code2000"
                                                         ))
    ("Cherokee Supplement"                              (                               ; todo free alternative
                                                         "Everson Mono:weight=bold"     ; 80/80
                                                         ))
    ("CJK Compatibility"                                (
                                                         "SimHei"
                                                         "FangSong"
                                                         "SimSun"
                                                         "MingLiU"
                                                         "Meiryo"                       ; 249/256
                                                         "Microsoft JhengHei"
                                                         "Microsoft JhengHei UI"
                                                         "Lantinghei SC"
                                                         "Lantinghei TC"
                                                         "HAN NOM A"                    ; 249/256
                                                         "Arial Unicode MS"
                                                         "WenQuanYi Zen Hei Mono"       ; 154/256
                                                         "HanaMinA"                     ; 256/256
                                                         "BabelStone Han"               ;  73/256
                                                         "Code2000"
                                                         ))
    ("CJK Compatibility Forms"                          (
                                                         "WenQuanYi Zen Hei Mono"       ; 32/32
                                                         "Lantinghei SC"
                                                         "SimHei"
                                                         "FangSong"
                                                         "SimSun"
                                                         "LiSong Pro"                   ; 26/32
                                                         "Baoli SC"                     ; 19/32
                                                         "Microsoft YaHei"
                                                         "Microsoft YaHei UI"
                                                         "Lantinghei TC"
                                                         "BabelStone Han"               ; 32/32
                                                         "MingLiU"
                                                         "Microsoft JhengHei"
                                                         "Microsoft JhengHei UI"
                                                         "HAN NOM A"                    ; 32/32
                                                         "Symbola"                      ; 32/32  @8.00 @8
                                                         "Xingkai SC"                   ; 19/32
                                                         "DFKai-SB"
                                                         "Code2000"
                                                         ))
    ("CJK Compatibility Ideographs"                     (
                                                         "SimHei"
                                                         "FangSong"
                                                         "SimSun"
                                                         "Microsoft YaHei"
                                                         "Microsoft YaHei UI"
                                                         "WenQuanYi Zen Hei Mono"       ; 455/472
                                                         "BabelStone Han"               ; 472/472
                                                         "UnBatang"                     ; 268/472
                                                         "MingLiU"
                                                         "Microsoft JhengHei"
                                                         "Microsoft JhengHei UI"
                                                         "HAN NOM A"                    ; 394/472
                                                         "Arial Unicode MS"
                                                         "Lantinghei SC"
                                                         "HanaMinA"                     ; 472/472
                                                         ))
    ("CJK Compatibility Ideographs Supplement"          (
                                                         "WenQuanYi Zen Hei Mono"       ; 542/542
                                                         "SimHei"
                                                         "FangSong"
                                                         "SimSun"
                                                         "MingLiU"
                                                         "HanaMinA"                     ; 542/542
                                                         "Hiragino Kaku Gothic Pro"
                                                         "Hiragino Maru Gothic Pro"
                                                         "Hiragino Mincho Pro"
                                                         "Microsoft JhengHei"
                                                         "Microsoft JhengHei UI"
                                                         "HAN NOM B"                    ; 542/542
                                                         "LiSong Pro"                   ;  11/542
                                                         ))
    ("CJK Radicals Supplement"                          (
                                                         "WenQuanYi Zen Hei Mono"       ; 115/115
                                                         "SimHei"
                                                         "FangSong"
                                                         "SimSun"
                                                         "Microsoft YaHei"
                                                         "Microsoft YaHei UI"
                                                         "HanaMinA"                     ; 115/115
                                                         "BabelStone Han"               ; 115/115
                                                         "MingLiU"
                                                         "Microsoft JhengHei"
                                                         "Microsoft JhengHei UI"
                                                         "HAN NOM A"                    ; 115/115
                                                         "DFKai-SB"
                                                         "Apple Symbols"                ; 115/115
                                                         "Code2000"
                                                         ))
    ("CJK Strokes"                                      (
                                                         "WenQuanYi Zen Hei Mono"       ; 36/36
                                                         "HanaMinA"                     ; 36/36
                                                         "BabelStone Han"               ; 36/36
                                                         "Code2000"
                                                         ))
    ("CJK Symbols and Punctuation"                      (
                                                         "Lantinghei SC"
                                                         "SimHei"
                                                         "FangSong"
                                                         "SimSun"
                                                         "HanaMinA"                     ; 58/64
                                                         "WenQuanYi Zen Hei Mono"       ; 38/64
                                                         "LiSong Pro"                   ; 33/64
                                                         "STFangsong"                   ; 35/64
                                                         "Microsoft YaHei"
                                                         "Microsoft YaHei UI"
                                                         "Lantinghei TC"
                                                         "MingLiU"
                                                         "HAN NOM A"                    ; 60/64
                                                         "Arial Unicode MS"
                                                         "PCMyungjo"                    ; 30/64
                                                         "BabelStone Han"               ; 55/64
                                                         "Osaka:spacing=m"
                                                         "Code2000"
                                                         ))
    ("CJK Unified Ideographs"                           (
                                                         "WenQuanYi Zen Hei Mono"       ; 20,940/20,950
                                                         "Lantinghei SC"
                                                         "Songti SC"                    ; 20,921/20,950
                                                         "SimHei"
                                                         "FangSong"
                                                         "STFangsong"                   ; 20,910/20,950
                                                         "SimSun"
                                                         "LiSong Pro"                   ; 17,595/20,950
                                                         "Baoli SC"                     ;  7,103/20,950
                                                         "HanaMinA"                     ; 20,945/20,950
                                                         "BabelStone Han"               ; 20,950/20,950
                                                         "Apple LiGothic"               ; 13,060/20,950
                                                         "Lantinghei TC"
                                                         "MingLiU"
                                                         "Microsoft JhengHei"
                                                         "Microsoft JhengHei UI"
                                                         "HAN NOM A"                    ; 20,902/20,950
                                                         "DFKai-SB"
                                                         "Arial Unicode MS"
                                                         "Xingkai SC"                   ;  7,103/20,950
                                                         "GB18030 Bitmap"               ; 20,902/20,950
                                                         "UnBatang"                     ;  4,260/20,950
                                                         ))
    ("CJK Unified Ideographs Extension A"               (
                                                         "SimHei"
                                                         "FangSong"
                                                         "STFangsong"                   ; 6,582/6,582
                                                         "SimSun"
                                                         "Songti SC"                    ; 6,582/6,582
                                                         "Microsoft YaHei"
                                                         "Microsoft YaHei UI"
                                                         "MingLiU"
                                                         "Microsoft JhengHei"
                                                         "Microsoft JhengHei UI"
                                                         "HanaMinA"                     ; 6,582/6,582
                                                         "HAN NOM A"                    ; 6,582/6,582
                                                         "Code2000"
                                                         "DFKai-SB"
                                                         "BabelStone Han"               ; 1,639/6,582
                                                         "GB18030 Bitmap"               ; 6,578/6,582
                                                         ))
    ("CJK Unified Ideographs Extension B"               (
                                                         "SimHei"
                                                         "FangSong"
                                                         "SimSun"
                                                         "LiSong Pro"                   ;  1,640/42,711
                                                         "Microsoft YaHei"
                                                         "Microsoft YaHei UI"
                                                         "HanaMinB"                     ; 42,711/42,711
                                                         "HAN NOM B"                    ; 42,711/42,711
                                                         "Code2002"                     ; 20,158/42,711
                                                         "MingLiU"
                                                         "Microsoft JhengHei"
                                                         "Microsoft JhengHei UI"
                                                         "BabelStone Han"               ;  2,616/42,711
                                                         "DFKai-SB"
                                                         ))
    ("CJK Unified Ideographs Extension C"               (
                                                         "HanaMinB"                     ; 4,149/4,149
                                                         "BabelStone Han"               ; 1,542/4,149
                                                         "HAN NOM B"                    ;   106/4,149
                                                         ))
    ("CJK Unified Ideographs Extension D"               (
                                                         "HanaMinB"                     ; 222/222
                                                         "BabelStone Han"               ; 222/222
                                                         ))
    ("CJK Unified Ideographs Extension E"               (
                                                         "HanaMinB"                     ; 5,762/5,762
                                                         "BabelStone Han"               ; 1,125/5,762
                                                         ))
    ("Combining Diacritical Marks"                      (
                                                         "Monaco"                       ; 112/112
                                                         "Consolas"
                                                         "Noto Sans"                    ; 112/112
                                                         "Cambria Math"                 ; 110/112
                                                         "Charis SIL"                   ; 104/112
                                                         "Doulos SIL"                   ; 104/112
                                                         "Courier New"
                                                         "DejaVu Sans:width=condensed"
                                                         "DejaVu Sans Mono"             ;  67/112  @2.35 @2.34999084472656
                                                         "Cardo"                        ; 112/112
                                                         "Code2000"                     ; 112/112
                                                         "Gentium Plus"                 ; 108/112
                                                         "Junicode"                     ; 109/112
                                                         "Tahoma"
                                                         "Microsoft Sans Serif"
                                                         "Arial"
                                                         "Quivira"                      ; 112/112  @4.1 @3.80000305175781
                                                         "Symbola"                      ; 112/112  @8.00 @8
                                                         "Everson Mono"                 ; 112/112
                                                         "FreeMono"                     ; 112/112
                                                         "Arial Unicode MS"             ;  72/112
                                                         "ALPHABETUM Unicode"           ; 112/112
                                                         ))
    ("Combining Diacritical Marks Extended"             (
                                                         "Monlam Uni Sans Serif"        ; 15/15
                                                         ))
    ("Combining Diacritical Marks Supplement"           (
                                                         "Cardo"                        ; 28/58
                                                         "FreeSerif"                    ; 33/58
                                                         "Junicode"                     ; 26/58
                                                         "Doulos SIL"                   ; 14/58
                                                         "DejaVu Sans:width=condensed"  ;  6/58
                                                         "Noto Sans"                    ; 13/58
                                                         "Segoe UI"                     ; 13/58
                                                         "Code2000"                     ; 13/58
                                                         "Everson Mono"                 ; 43/58
                                                         "ALPHABETUM Unicode"           ; 43/58
                                                         ))
    ("Combining Diacritical Marks for Symbols"          (
                                                         "Cambria Math"                 ; 22/33
                                                         "Segoe UI Symbol"              ; 33/33
                                                         "Noto Sans Symbols"            ; 33/33
                                                         "Symbola"                      ; 33/33  @8.00 @8
                                                         "Code2000"                     ; 28/33
                                                         "Everson Mono"                 ; 33/33
                                                         "Arial Unicode MS"             ; 18/33
                                                         ))
    ("Combining Half Marks"                             (
                                                         "Consolas"                     ;  4/16
                                                         "DejaVu Sans:width=condensed"  ;  4/16
                                                         "Everson Mono:weight=bold"     ;  7/16
                                                         "Symbola"                      ; 16/16  @8.00 @8
                                                         ))
    ("Common Indic Number Forms"                        (
                                                         "Noto Sans Kaithi"             ; 10/10
                                                         "Nirmala UI"
                                                         "Siddhanta"
                                                         ))
    ("Control Pictures"                                 (
                                                         "Apple Symbols"                ; 39/39
                                                         "BabelStone Modern"            ; 39/39
                                                         "Noto Sans Symbols"            ; 39/39
                                                         "Segoe UI Symbol"              ; 39/39
                                                         "Arial Unicode MS"
                                                         "Symbola"                      ; 39/39  @8.00 @8
                                                         "Quivira"                      ; 39/39  @4.1 @3.80000305175781
                                                         "FreeMono"                     ; 39/39
                                                         "Code2000"
                                                         "Everson Mono:weight=bold"     ; 39/39
                                                         ))
    ("Coptic"                                           (
                                                         "Noto Sans Coptic"             ; 123/123
                                                         "Antinoou"                     ; 123/123
                                                         "New Athena Unicode"           ; 121/123
                                                         "Segoe UI Historic"            ; 123/123  @1.00 @1
                                                         "Segoe UI Symbol"              ; 123/123
                                                         "Quivira"                      ; 123/123  @4.1 @3.80000305175781
                                                         "Analecta"                     ; 123/123
                                                         "Nilus"                        ; 123/123
                                                         "Code2000"                     ; 114/123
                                                         "Everson Mono:weight=bold"     ; 123/123
                                                         "ALPHABETUM Unicode"           ; 123/123
                                                         ))
    ("Coptic Epact Numbers"                             (
                                                         "Nilus"                        ; 28/28
                                                         "Symbola"                      ; 28/28  @8.00 @8
                                                         ))
    ("Counting Rod Numerals"                            (
                                                         "WenQuanYi Zen Hei Mono"       ; 18/18
                                                         "Noto Sans Symbols"            ; 18/18
                                                         "BabelStone Modern"            ; 18/18
                                                         "Symbola"                      ; 18/18  @8.00 @8
                                                         "Quivira"                      ; 18/18  @4.1 @3.80000305175781
                                                         "Apple Symbols"                ; 18/18
                                                         "Code2001"                     ; 18/18
                                                         ))
    ("Cuneiform"                                        (
                                                         "Segoe UI Historic"                   ; 921/922  @1.00 @1
                                                         "Noto Sans Cuneiform"                 ; 879/922
                                                         "Noto Sans Sumero-Akkadian Cuneiform" ; 879/922 - old name
                                                         "Akkadian"
                                                         ))
    ("Cuneiform Numbers and Punctuation"                (
                                                         "Akkadian"
                                                         "Segoe UI Historic"                   ; 116/116  @1.00 @1
                                                         "Noto Sans Cuneiform"                 ; 103/116
                                                         "Noto Sans Sumero-Akkadian Cuneiform" ; 103/116 - old name
                                                         ))
    ("Currency Symbols"                                 (
                                                         "Monaco"                       ; 19/31
                                                         "DejaVu Sans Mono"             ; 24/31  @2.35 @2.34999084472656
                                                         "DejaVu Sans:width=condensed"  ; 24/31
                                                         "Consolas"                     ; 25/31
                                                         "Noto Sans Symbols"            ; 29/31
                                                         "Noto Sans"                    ; 23/31
                                                         "Segoe UI"                     ; 27/31
                                                         "Apple Symbols"                ; 22/31
                                                         "Symbola"                      ; 31/31  @8.00 @8
                                                         "Quivira"                      ; 30/31  @4.1 @3.80000305175781
                                                         "Everson Mono:weight=bold"     ; 27/31
                                                         "FreeMono"                     ; 23/31
                                                         ))
    ("Cypriot Syllabary"                                (
                                                         "Segoe UI Historic"            ; 55/55  @1.00 @1
                                                         "Noto Sans Cypriot"            ; 55/55
                                                         "Aegean"                       ; 55/55
                                                         "Code2001"                     ; 55/55
                                                         "Everson Mono:weight=bold"     ; 55/55
                                                         "ALPHABETUM Unicode"           ; 55/55
                                                         ))
    ("Cyrillic"                                         (
                                                         "Consolas"                     ; 255/256
                                                         "Monaco"                       ; 191/256
                                                         "DejaVu Sans Mono"             ; 180/256  @2.35 @2.34999084472656
                                                         "DejaVu Sans:width=condensed"  ; 256/256
                                                         "Noto Sans"                    ; 256/256
                                                         "Courier New"                  ; 118/256
                                                         "Calibri"                      ; 255/256
                                                         "Microsoft Sans Serif"         ; 246/256
                                                         "Code2000"                     ; 256/256
                                                         "Arial Unicode MS"             ; 226/256
                                                         "Charis SIL"                   ; 220/256
                                                         "Doulos SIL"                   ; 220/256
                                                         "Symbola"                      ; 256/256  @8.00 @8
                                                         "Quivira"                      ; 256/256  @4.1 @3.80000305175781
                                                         "Everson Mono:weight=bold"     ; 256/256
                                                         "FreeMono"                     ; 251/256
                                                         "Charcoal CY"                  ;  94/256
                                                         "Geneva CY"                    ;  94/256
                                                         "ALPHABETUM Unicode"           ; 256/256
                                                         ))
    ("Cyrillic Extended-A"                              (
                                                         "Quivira"                      ; 32/32  @4.1 @3.80000305175781
                                                         "Everson Mono:weight=bold"     ; 32/32
                                                         "FreeSerif"                    ; 32/32
                                                         "ALPHABETUM Unicode"           ; 32/32
                                                         ))
    ("Cyrillic Extended-B"                              (
                                                         "Quivira"                      ; 95/96  @4.1 @3.80000305175781
                                                         "Code2000"                     ; 78/96
                                                         "Everson Mono:weight=bold"     ; 95/96
                                                         ))
    ("Cyrillic Supplement"                              (
                                                         "Consolas"                     ; 20/48
                                                         "Courier New"
                                                         "Calibri"
                                                         "Noto Sans"                    ; 40/48
                                                         "DejaVu Sans:width=condensed"
                                                         "Charis SIL"                   ; 40/48
                                                         "Doulos SIL"                   ; 40/48
                                                         "Symbola"                      ; 48/48  @8.00 @8
                                                         "Quivira"                      ; 48/48  @4.1 @3.80000305175781
                                                         "Code2000"                     ; 36/48
                                                         "Everson Mono:weight=bold"     ; 48/48
                                                         ))
    ("Deseret"                                          (
                                                         "Noto Sans Deseret"            ; 80/80
                                                         "Apple Symbols"                ; 80/80
                                                         "Segoe UI Symbol"              ; 80/80
                                                         "Analecta"
                                                         "Code2001"                     ; 80/80
                                                         "Everson Mono:weight=bold"     ; 80/80
                                                         ))
    ("Devanagari"                                       (
                                                         "Annapurna SIL"                ; 128/128  @1.200 @1.19999694824219
                                                         "Noto Sans Devanagari"         ; 112/128
                                                         "Devanagari Sangam MN"
                                                         "Devanagari MT"
                                                         "Nirmala UI"                   ; 113/128
                                                         "Mangal"                       ; 112/128
                                                         "Samyak Devanagari"            ; 112/128
                                                         "Samyak"                       ; 115/128
                                                         "Siddhanta"                    ; 127/128
                                                         "Aparajita"                    ; 127/128
                                                         "Code2000"                     ; 112/128
                                                         "Arial Unicode MS"             ; 104/128
                                                         "ALPHABETUM Unicode"           ; 117/128
                                                         ))
    ("Devanagari Extended"                              (
                                                         "Annapurna SIL"                ; 28/30  @1.200 @1.19999694824219
                                                         "Siddhanta"                    ; 28/30
                                                         "FreeSerif"                    ; 28/30
                                                         ))
    ("Dingbats"                                         (
                                                         "Apple Color Emoji"
                                                         "DejaVu Sans Mono"             ; 144/192  @2.35 @2.34999084472656
                                                         "Segoe UI Symbol"              ; 191/192
                                                         "Zapf Dingbats"                ; 174/192
                                                         "DejaVu Sans:width=condensed"  ; 174/192
                                                         "Arial Unicode MS"             ; 160/192
                                                         "Code2000"                     ; 174/192
                                                         "Noto Sans Symbols"            ; 191/192
                                                         "Symbola"                      ; 192/192  @8.00 @8
                                                         "Quivira"                      ; 160/192  @4.1 @3.80000305175781
                                                         "Everson Mono:weight=bold"     ; 192/192
                                                         ))
    ("Domino Tiles"                                     (
                                                         "DejaVu Sans:width=condensed"
                                                         "Symbola"                      ; 100/100  @8.00 @8
                                                         "Quivira"                      ; 100/100  @4.1 @3.80000305175781
                                                         "Segoe UI Symbol"              ; 100/100
                                                         "Noto Sans Symbols"            ; 100/100
                                                         "Code2001"                     ; 100/100
                                                         "Everson Mono:weight=bold"     ; 100/100
                                                         ))
    ;; ("Duployan"                                      (""))                           ; todo added in Unicode 7.0
    ("Early Dynastic Cuneiform"                         (
                                                         "Akkadian"                     ; 196/196
                                                         ))
    ("Egyptian Hieroglyphs"                             (
                                                         "Segoe UI Historic:weight=bold"              ; 1,071/1,071  @1.00 @1
                                                         "Noto Sans Egyptian Hieroglyphs:weight=bold" ; 1,071/1,071
                                                         "Aegyptus:weight=bold"                       ; 1,071/1,071
                                                         "Gardiner"                                   ; 1,071/1,071
                                                         ))
    ("Elbasan"                                          (
                                                         "Albanian"                     ; 40/40
                                                         "Everson Mono:weight=bold"     ; 40/40
                                                         ))
    ("Emoticons"                                        (
                                                         "Apple Color Emoji"
                                                         "Segoe UI Symbol"              ; 76/80
                                                         "Symbola"                      ; 80/80  @8.00 @8
                                                         "Quivira"                      ; 78/80  @4.1 @3.80000305175781
                                                         ))
    ("Enclosed Alphanumeric Supplement"                 (
                                                         "Segoe UI Symbol"              ; 169/173
                                                         "Noto Sans Symbols"            ; 171/173
                                                         "Symbola"                      ; 173/173  @8.00 @8
                                                         "Quivira"                      ; 173/173  @4.1 @3.80000305175781
                                                         "BabelStone Han"               ; 173/173
                                                         "BabelStone Modern"            ; 169/173
                                                         ))
    ("Enclosed Alphanumerics"                           (
                                                         ;; "Aqua Kana"
                                                         "Noto Sans Symbols"            ; 160/160
                                                         "Segoe UI Symbol"              ; 160/160
                                                         "Junicode"                     ; 160/160
                                                         "Arial Unicode MS"             ; 139/160
                                                         "Symbola"                      ; 160/160  @8.00 @8
                                                         "Quivira"                      ; 160/160  @4.1 @3.80000305175781
                                                         "Code2000"                     ; 160/160
                                                         "BabelStone Han"               ; 160/160
                                                         "WenQuanYi Zen Hei Mono"       ; 160/160
                                                         "BabelStone Modern"            ; 160/160
                                                         "HAN NOM A"                    ; 139/160
                                                         "Everson Mono:weight=bold"     ; 160/160
                                                         ))
    ("Enclosed CJK Letters and Months"                  (
                                                         "WenQuanYi Zen Hei Mono"       ; 202/254
                                                         "SimHei"
                                                         "FangSong"
                                                         "MingLiU"
                                                         ;; "Aqua Kana"
                                                         "Arial Unicode MS"
                                                         "HanaMinA"                     ; 254/254
                                                         "Meiryo"                       ; 174/254
                                                         "BabelStone Han"               ; 191/254
                                                         "Quivira"                      ;  39/254  @4.1 @3.80000305175781
                                                         "Code2000"
                                                         "UnBatang"                     ;  58/254
                                                         "HAN NOM A"                    ; 232/254
                                                         ))
    ("Enclosed Ideographic Supplement"                  (
                                                         "Segoe UI Symbol"              ; 57/57
                                                         "Noto Sans Symbols"            ; 57/57
                                                         "HanaMinA"                     ; 57/57
                                                         "BabelStone Han"               ; 57/57
                                                         "Symbola"                      ; 57/57  @8.00 @8
                                                         ))
    ("Ethiopic"                                         (
                                                         "Kefa"
                                                         "Noto Sans Ethiopic"           ; 358/358
                                                         "Nyala"
                                                         "Abyssinica SIL"               ; 358/358
                                                         "Ethiopia Jiret"               ; 345/358
                                                         "Ethiopic WashRa SemiBold"     ; 345/358
                                                         "Ethiopic Yebse"               ; 342/358
                                                         "Code2000"                     ; 356/358
                                                         ))
    ("Ethiopic Extended"                                (
                                                         "Kefa"
                                                         "Noto Sans Ethiopic"           ; 79/79
                                                         "Nyala"
                                                         "Abyssinica SIL"               ; 79/79
                                                         "Code2000"                     ; 79/79
                                                         ))
    ("Ethiopic Extended-A"                              (
                                                         "Kefa"
                                                         "Noto Sans Ethiopic"           ; 32/32
                                                         "Abyssinica SIL"               ; 32/32
                                                         ))
    ("Ethiopic Supplement"                              (
                                                         "Kefa"
                                                         "Noto Sans Ethiopic"           ; 26/26
                                                         "Nyala"
                                                         "Abyssinica SIL"               ; 26/26
                                                         "Code2000"                     ; 26/26
                                                         ))
    ("General Punctuation"                              (
                                                         "Monaco"                       ; 106/111
                                                         "Apple Symbols"                ; 106/111
                                                         "Segoe UI Symbol"              ; 107/111
                                                         "Cambria Math"                 ;  36/111
                                                         "DejaVu Sans Mono"             ;  54/111  @2.35 @2.34999084472656
                                                         "DejaVu Sans:width=condensed"  ; 107/111
                                                         "Charis SIL"                   ;  74/111
                                                         "Doulos SIL"                   ;  74/111
                                                         "Antinoou"                     ; 106/111
                                                         "Symbola"                      ; 111/111  @8.00 @8
                                                         "Code2000"                     ; 106/111
                                                         "Quivira"                      ; 105/111  @4.1 @3.80000305175781
                                                         "Noto Sans"                    ;  56/111
                                                         "Everson Mono:weight=bold"     ; 107/111
                                                         "FreeMono"                     ; 101/111
                                                         "BabelStone Modern"            ;  96/111
                                                         ))
    ("Geometric Shapes"                                 (
                                                         "DejaVu Sans Mono"             ; 96/96  @2.35 @2.34999084472656
                                                         "DejaVu Sans:width=condensed"  ; 96/96
                                                         "Segoe UI Symbol"              ; 96/96
                                                         "Arial Unicode MS"             ; 80/96
                                                         "Symbola"                      ; 96/96  @8.00 @8
                                                         "Noto Sans Symbols"            ; 96/96
                                                         "Quivira"                      ; 96/96  @4.1 @3.80000305175781
                                                         "BabelStone Modern"            ; 96/96
                                                         "Everson Mono"                 ; 96/96
                                                         "FreeMono"                     ; 96/96
                                                         "Code2000"
                                                         ))
    ("Geometric Shapes Extended"                        (
                                                         "Symbola"                      ; 85/85  @8.00 @8
                                                         "Quivira"                      ; 64/85  @4.1 @3.80000305175781
                                                         ))
    ("Georgian"                                         (
                                                         "DejaVu Sans Mono"             ; 45/88  @2.35 @2.34999084472656
                                                         "Noto Sans Georgian"           ; 83/88
                                                         "Noto Serif Georgian"          ; 83/88
                                                         "DejaVu Sans:width=condensed"  ; 83/88
                                                         "Arial Unicode MS"             ; 78/88
                                                         "Code2000"                     ; 83/88
                                                         "Quivira"                      ; 88/88  @4.1 @3.80000305175781
                                                         "Sylfaen"                      ; 40/88
                                                         "MPH 2B Damase"                ; 39/88
                                                         "Everson Mono:weight=bold"     ; 88/88
                                                         ))
    ("Georgian Supplement"                              (
                                                         "Noto Sans Georgian"           ; 38/40
                                                         "Noto Serif Georgian"          ; 38/40
                                                         "DejaVu Serif:width=condensed" ; 38/40
                                                         "MPH 2B Damase"                ; 38/40
                                                         "Quivira"                      ; 40/40  @4.1 @3.80000305175781
                                                         "Everson Mono:weight=bold"     ; 40/40
                                                         ))
    ("Glagolitic"                                       (
                                                         "Noto Sans Glagolitic"         ; 94/94
                                                         "Segoe UI Historic"            ; 94/94  @1.00 @1
                                                         "Segoe UI Symbol"              ; 94/94
                                                         "MPH 2B Damase"
                                                         "Quivira"                      ; 94/94  @4.1 @3.80000305175781
                                                         "FreeSerif"                    ; 86/94
                                                         "ALPHABETUM Unicode"           ; 94/94
                                                         ))
    ("Gothic"                                           (
                                                         "Noto Sans Gothic"             ; 27/27
                                                         "Segoe UI Historic"            ; 27/27  @1.00 @1
                                                         "Segoe UI Symbol"              ; 27/27
                                                         "Analecta"                     ; 27/27
                                                         "Junicode"                     ; 27/27
                                                         "Sadagolthina"                 ; 27/27
                                                         "MPH 2B Damase"                ; 27/27
                                                         "FreeSerif"                    ; 27/27
                                                         "Code2001"                     ; 27/27
                                                         "Quivira"                      ; 27/27  @4.1 @3.80000305175781
                                                         "Everson Mono:weight=bold"     ; 27/27
                                                         "ALPHABETUM Unicode"           ; 27/27
                                                         ))
    ;; ("Grantha"                                       (""))                           ; todo added in Unicode 7.0
    ("Greek Extended"                                   (
                                                         "Consolas"                     ; 232/233
                                                         "DejaVu Sans Mono"             ; 233/233  @2.35 @2.34999084472656
                                                         "Courier New"
                                                         "Antinoou"                     ; 233/233
                                                         "Noto Sans"                    ; 233/233
                                                         "DejaVu Sans:width=condensed"
                                                         "Cardo"                        ; 233/233
                                                         "Junicode"                     ; 233/233
                                                         "New Athena Unicode"           ; 233/233
                                                         "Microsoft Sans Serif"
                                                         "Gentium Plus Compact"
                                                         "Gentium Plus"
                                                         "Arial Unicode MS"
                                                         "Arial"
                                                         "Tahoma"
                                                         "Aegean"                       ; 233/233
                                                         "Code2000"
                                                         "Quivira"                      ; 233/233  @4.1 @3.80000305175781
                                                         "Everson Mono:weight=bold"     ; 233/233
                                                         "FreeMono"                     ; 233/233
                                                         "ALPHABETUM Unicode"           ; 233/233
                                                         ))
    ("Greek and Coptic"                                 (
                                                         "Consolas"                     ;  75/135
                                                         "DejaVu Sans Mono"             ; 110/135  @2.35 @2.34999084472656
                                                         "DejaVu Sans:width=condensed"  ; 134/135
                                                         "Antinoou"                     ; 135/135
                                                         "Noto Sans"                    ; 127/135
                                                         "Segoe UI Historic"            ; 83/135  @1.00 @1
                                                         "Segoe UI Symbol"              ; 134/135
                                                         "New Athena Unicode"           ; 134/135
                                                         "Calibri"                      ; 127/135
                                                         "Microsoft Sans Serif"         ; 112/135
                                                         "Gentium Plus Compact"
                                                         "Gentium Plus"
                                                         "Lucida Console"               ;  73/135
                                                         "Arial Unicode MS"             ; 105/135
                                                         "Cardo"                        ; 134/135
                                                         "Aegean"                       ; 134/135
                                                         "Code2000"
                                                         "Symbola"                      ; 135/135  @8.00 @8
                                                         "Quivira"                      ; 135/135  @4.1 @3.80000305175781
                                                         "Everson Mono:weight=bold"     ; 135/135
                                                         "ALPHABETUM Unicode"           ; 134/135
                                                         "Noto Sans Coptic"             ;  14/135
                                                         ))
    ("Gujarati"                                         (
                                                         "Nirmala UI"                   ; 83/85
                                                         "Noto Sans Gujarati"           ; 84/85
                                                         "Noto Sans Gujarati UI"        ; 84/85
                                                         "Gujarati MT"
                                                         "Shruti"                       ; 83/85
                                                         "Samyak Gujarati"
                                                         "Samyak"                       ; 84/85
                                                         "Gujarati Sangam MN"
                                                         "Code2000"                     ; 83/85
                                                         "Arial Unicode MS"             ; 78/85
                                                         ))
    ("Gurmukhi"                                         (
                                                         "Gurmukhi Sangam MN"
                                                         "Gurmukhi MN"
                                                         "Nirmala UI"                   ; 79/79
                                                         "Noto Sans Gurmukhi"           ; 79/79
                                                         "Noto Sans Gurmukhi UI"        ; 79/79
                                                         "Raavi"                        ; 79/79
                                                         "Code2000"                     ; 79/79
                                                         "Arial Unicode MS"             ; 75/79
                                                         "AnmolUni"                     ; 77/79
                                                         ))
    ("Halfwidth and Fullwidth Forms"                    (
                                                         "Meiryo"                       ; 166/225
                                                         "Arial Unicode MS"             ; 223/225
                                                         "Microsoft JhengHei"           ; 225/225
                                                         "Microsoft JhengHei UI"        ; 225/225
                                                         "Microsoft YaHei"              ; 224/225
                                                         "Microsoft YaHei UI"           ; 224/225
                                                         "BabelStone Han"               ; 173/225
                                                         "Apple Symbols"                ;  55/225
                                                         "Quivira"                      ; 110/225  @4.1 @3.80000305175781
                                                         "Code2000"                     ; 186/225
                                                         "HAN NOM A"                    ; 170/225
                                                         ))
    ("Hangul Compatibility Jamo"                        (
                                                         "PCMyungjo"                    ; 94/94
                                                         "Malgun Gothic"
                                                         "Gulim"
                                                         "Dotum"
                                                         "Batang"
                                                         "Gungsuh"
                                                         "AppleMyungjo"                 ; 94/94
                                                         "UnBatang"                     ; 94/94
                                                         "WenQuanYi Zen Hei Mono"       ; 94/94
                                                         "HAN NOM A"                    ; 93/94
                                                         "Arial Unicode MS"
                                                         "Code2000"
                                                         "HeadLineA"                    ; 94/94
                                                         ))
    ("Hangul Jamo"                                      (
                                                         "UnBatang"                     ; 256/256
                                                         "WenQuanYi Zen Hei Mono"       ; 146/256
                                                         "PCMyungjo"                    ;  67/256
                                                         "Malgun Gothic"
                                                         "Gulim"
                                                         "Dotum"
                                                         "Batang"
                                                         "Gungsuh"
                                                         "Arial Unicode MS"
                                                         "Code2000"
                                                         ))
    ("Hangul Jamo Extended-A"                           (
                                                         "Malgun Gothic"                ; 29/29
                                                         "HanaMinA"                     ; 29/29
                                                         "UnBatang"                     ; 29/29
                                                         ))
    ("Hangul Jamo Extended-B"                           (
                                                         "Malgun Gothic"                ; 72/72
                                                         "HanaMinA"                     ; 72/72
                                                         "UnBatang"                     ; 72/72
                                                         ))
    ("Hangul Syllables"                                 (
                                                         "AppleGothic"                  ; 11,172/11,172
                                                         "Malgun Gothic"
                                                         "Gulim"
                                                         "Dotum"
                                                         "Batang"
                                                         "Gungsuh"
                                                         "UnBatang"                     ; 11,172/11,172
                                                         "WenQuanYi Zen Hei Mono"       ; 11,172/11,172
                                                         "Arial Unicode MS"
                                                         "Code2000"
                                                         ))
    ("Hanunoo"                                          (
                                                         "Noto Sans Hanunoo"            ; 23/23
                                                         "MPH 2B Damase"
                                                         "Quivira"                      ; 23/23  @4.1 @3.80000305175781
                                                         "FreeSerif"                    ; 23/23
                                                         ))
    ;; ("Hatran"                                        (""))                           ; todo added in Unicode 8.0
    ("Hebrew"                                           (
                                                         "Miriam Fixed"
                                                         "Ezra SIL"                     ; 87/87
                                                         "Ezra SIL SR"                  ; 87/87
                                                         "Arial Hebrew"
                                                         "Raanana"
                                                         "New Peninim MT"
                                                         "Aharoni"
                                                         "David"
                                                         "FrankRuehl"
                                                         "Gisha"
                                                         "Levenim MT"
                                                         "Narkisim"
                                                         "Rod"
                                                         "Cardo"                        ; 87/87
                                                         "Courier New"
                                                         "Adobe Hebrew"                 ; 54/87
                                                         "Code2000"
                                                         "Aramaic Imperial Yeb"         ; 28/87, uncommon characters
                                                         "Microsoft Sans Serif"
                                                         "Tahoma"
                                                         "Lucida Sans Unicode"
                                                         "Arial Unicode MS"
                                                         "Arial"
                                                         "Quivira"                      ; 87/87  @4.1 @3.80000305175781
                                                         "Everson Mono:weight=bold"     ; 87/87
                                                         "ALPHABETUM Unicode"           ; 87/87
                                                         ))
    ;; ("High Private Use Surrogates"                   (""))                           ; no displayable characters
    ;; ("High Surrogates"                               (""))                           ; no displayable characters
    ("Hiragana"                                         (
                                                         "Osaka:spacing=m"
                                                         "MS Gothic"
                                                         "MS Mincho"
                                                         "MingLiU"
                                                         "Hiragino Kaku Gothic Pro"
                                                         ;; "Aqua Kana"
                                                         "Meiryo"
                                                         "Arial Unicode MS"
                                                         "HanaMinA"                     ; 93/93
                                                         "BabelStone Han"               ; 93/93
                                                         "Microsoft JhengHei"           ; 93/93
                                                         "Microsoft YaHei"              ; 93/93
                                                         "Microsoft YaHei UI"           ; 93/93
                                                         "HAN NOM A"                    ; 88/93
                                                         "Code2000"
                                                         "ALPHABETUM Unicode"           ; 92/93
                                                         ))
    ("Ideographic Description Characters"               (
                                                         "SimHei"
                                                         "FangSong"
                                                         "SimSun"
                                                         "Microsoft YaHei"
                                                         "Microsoft YaHei UI"
                                                         "BabelStone Han"               ; 12/12
                                                         "MingLiU"
                                                         "Microsoft JhengHei"
                                                         "Microsoft JhengHei UI"
                                                         "AppleMyungjo"
                                                         "HanaMinA"                     ; 12/12
                                                         "HAN NOM A"                    ; 12/12
                                                         "Quivira"                      ; 12/12  @4.1 @3.80000305175781
                                                         "DFKai-SB"
                                                         "Code2000"
                                                         ))
    ("Imperial Aramaic"                                 (
                                                         "Aramaic Imperial Yeb"         ; 31/31
                                                         "Quivira"                      ; 31/31  @4.1 @3.80000305175781
                                                         "Segoe UI Historic"            ; 31/31  @1.00 @1
                                                         "Noto Sans Imperial Aramaic"   ; 31/31
                                                         "Everson Mono:weight=bold"     ; 31/31
                                                         "ALPHABETUM Unicode"           ; 31/31
                                                         ))
    ("IPA Extensions"                                   (
                                                         "Monaco"                       ; 78/96
                                                         "Consolas"                     ; 96/96
                                                         "DejaVu Sans Mono"             ; 96/96  @2.35 @2.34999084472656
                                                         "Courier New"
                                                         "Noto Sans"                    ; 96/96
                                                         "Arial Unicode MS"
                                                         "Arial"
                                                         "Tahoma"
                                                         "Microsoft Sans Serif"
                                                         "Aboriginal Sans"              ; 91/96
                                                         "Cardo"                        ; 96/96
                                                         "Symbola"                      ; 96/96  @8.00 @8
                                                         "Quivira"                      ; 96/96  @4.1 @3.80000305175781
                                                         "Everson Mono:weight=bold"     ; 96/96
                                                         "FreeMono"                     ; 96/96
                                                         "Code2000"
                                                         "ALPHABETUM Unicode"           ; 96/96
                                                         ))
    ("Inscriptional Pahlavi"                            (
                                                         "ZH Mono"                             ; 27/27
                                                         "Segoe UI Historic"                   ; 27/27  @1.00 @1
                                                         "Noto Sans Inscriptional Pahlavi"     ; 27/27
                                                         "ALPHABETUM Unicode"                  ; 27/27
                                                         "Ahuramzda:weight=bold"               ;  8/27
                                                         ))
    ("Inscriptional Parthian"                           (
                                                         "ZH Mono"                             ; 30/30
                                                         "Segoe UI Historic"                   ; 30/30  @1.00 @1
                                                         "Noto Sans Inscriptional Parthian"    ; 30/30
                                                         "ALPHABETUM Unicode"                  ; 30/30
                                                         ))
    ("Javanese"                                         (
                                                         "Noto Sans Javanese"                  ; 91/91
                                                         "Tuladha Jejeg"
                                                         ))
    ("Kaithi"                                           (
                                                         "Noto Sans Kaithi"             ; 66/66
                                                         ))
    ("Kana Supplement"                                  (
                                                         "Meiryo UI"                    ; 2/2
                                                         "HanaMinA"                     ; 2/2
                                                         "BabelStone Han"               ; 2/2
                                                         ))
    ("Kanbun"                                           (
                                                         "SimHei"
                                                         "FangSong"
                                                         "SimSun"
                                                         "Meiryo"
                                                         ;; "Aqua Kana"
                                                         "Arial Unicode MS"
                                                         "WenQuanYi Zen Hei Mono"       ; 14/16
                                                         "HanaMinA"                     ; 16/16
                                                         "BabelStone Han"               ; 16/16
                                                         "MingLiU"
                                                         "Microsoft JhengHei"           ; 16/16
                                                         "Microsoft YaHei"              ; 16/16
                                                         "Microsoft YaHei UI"           ; 16/16
                                                         "HAN NOM A"                    ; 14/16
                                                         "Code2000"
                                                         ))
    ("Kangxi Radicals"                                  (
                                                         "WenQuanYi Zen Hei Mono"       ; 214/214
                                                         "SimHei"
                                                         "FangSong"
                                                         "Meiryo"                       ; 214/214
                                                         "SimSun"
                                                         "Microsoft YaHei"
                                                         "Microsoft YaHei UI"
                                                         "BabelStone Han"               ; 214/214
                                                         "HanaMinA"                     ; 214/214
                                                         "MingLiU"
                                                         "Microsoft JhengHei"
                                                         "Microsoft JhengHei UI"
                                                         "HAN NOM A"                    ; 214/214
                                                         "DFKai-SB"
                                                         "AppleMyungjo"
                                                         "Apple Symbols"                ; 214/214
                                                         "Code2000"
                                                         ;; "Aqua Kana"
                                                         ))
    ("Kannada"                                          (
                                                         "Kannada Sangam MN"
                                                         "Noto Sans Kannada"            ; 86/87
                                                         "Noto Sans Kannada UI"         ; 86/87
                                                         "Tunga"                        ; 86/87
                                                         "Akshar Unicode"               ; 82/87
                                                         "Kedage"                       ; 78/87
                                                         "Nirmala UI"                   ; 86/87
                                                         "Kannada MN"
                                                         "Arial Unicode MS"             ; 80/87
                                                         "Code2000"                     ; 86/87
                                                         ))
    ("Katakana"                                         (
                                                         "Osaka:spacing=m"
                                                         ;; "Aqua Kana"
                                                         "MS Gothic"
                                                         "MingLiU"
                                                         "Meiryo"
                                                         "HanaMinA"                     ; 96/96
                                                         "Arial Unicode MS"
                                                         "BabelStone Han"               ; 96/96
                                                         "Microsoft JhengHei"           ; 96/96
                                                         "Microsoft YaHei"              ; 96/96
                                                         "Microsoft YaHei UI"           ; 96/96
                                                         "HAN NOM A"                    ; 94/96
                                                         "Code2000"
                                                         "ALPHABETUM Unicode"           ; 96/96
                                                         ))
    ("Katakana Phonetic Extensions"                     (
                                                         "MS Gothic"
                                                         "MingLiU"
                                                         ;; "Aqua Kana"
                                                         "Meiryo"
                                                         "HanaMinA"                     ; 16/16
                                                         "Microsoft YaHei"              ; 16/16
                                                         "Microsoft YaHei UI"           ; 16/16
                                                         "BabelStone Han"               ; 16/16
                                                         "HAN NOM A"                    ; 16/16
                                                         "Code2000"
                                                         ))
    ("Kayah Li"                                         (
                                                         "Noto Sans Kayah Li"           ; 48/48
                                                         "Code2000"
                                                         "FreeMono"                     ; 48/48
                                                         ))
    ("Kharoshthi"                                       (
                                                         "Segoe UI Historic"            ; 65/65  @1.00 @1
                                                         "Noto Sans Kharoshthi"         ; 65/65
                                                         "MPH 2B Damase"
                                                         "ALPHABETUM Unicode"           ; 65/65
                                                         ))
    ("Khmer"                                            (
                                                         "Noto Sans Khmer"              ; 114/114
                                                         "Noto Sans Khmer UI"           ; 114/114
                                                         "Noto Serif Khmer"             ; 114/114
                                                         "Khmer Sangam MN"
                                                         "DaunPenh"                     ; 114/114
                                                         "Code2000"                     ; 114/114
                                                         "MoolBoran"                    ; 114/114
                                                         "Khmer Mondulkiri"             ; 114/114
                                                         "Khmer Busra"                  ; 114/114
                                                         ))
    ("Khmer Symbols"                                    (
                                                         "Noto Sans Khmer"              ; 32/32
                                                         "Noto Sans Khmer UI"           ; 32/32
                                                         "Noto Serif Khmer"             ; 32/32
                                                         "Khmer Sangam MN"
                                                         "MoolBoran"                    ; 32/32
                                                         "Khmer Mondulkiri"             ; 32/32
                                                         "Khmer Busra"                  ; 32/32
                                                         "Code2000"                     ; 32/32
                                                         ))
    ("Khojki"                                           (
                                                         "KhojkiUnicodeOT"              ; 61/61
                                                         ))
    ("Khudawadi"                                        (
                                                         "OldSindhi"                    ; 69/69
                                                         ))
    ("Lao"                                              (
                                                         "Noto Sans Lao"                ; 67/67
                                                         "Noto Sans Lao UI"             ; 67/67
                                                         "Noto Serif Lao"               ; 67/67
                                                         "Lao Sangam MN"
                                                         "DokChampa"                    ; 65/67
                                                         "DejaVu Sans Mono"             ; 46/67  @2.35 @2.34999084472656
                                                         "Arial Unicode MS"             ; 65/67
                                                         "Saysettha MX"                 ; 65/67
                                                         "DejaVu Sans:width=condensed"  ; 65/67
                                                         "Code2000"                     ; 65/67
                                                         ))
    ;; ("Latin Extended Additional"                     (                               ; hopefully well-covered by the default font
    ;;                                                   "Monaco"
    ;;                                                   "DejaVu Sans Mono"
    ;;                                                   "Courier New"
    ;;                                                   "Quivira"
    ;;                                                   "DejaVu Sans:width=condensed"
    ;;                                                  ))
    ;; ("Latin Extended-A"                              (                               ; hopefully well-covered by the default font
    ;;                                                   "Monaco"
    ;;                                                   "Consolas"
    ;;                                                   "DejaVu Sans Mono"
    ;;                                                   "Courier New"
    ;;                                                   "DejaVu Sans:width=condensed"
    ;;                                                   "Quivira"
    ;;                                                  ))
    ;; ("Latin Extended-B"                              (                               ; hopefully well-covered by the default font
    ;;                                                   "Monaco"                       ; fairly incomplete
    ;;                                                   "Consolas"
    ;;                                                   "DejaVu Sans:width=condensed"
    ;;                                                   "DejaVu Sans Mono"
    ;;                                                   "Courier New"
    ;;                                                   "Quivira"
    ;;                                                  ))
    ("Latin Extended-C"                                 (
                                                         "DejaVu Sans Mono"             ; 14/32  @2.35 @2.34999084472656
                                                         "DejaVu Sans:width=condensed"
                                                         "Noto Sans"                    ; 21/32
                                                         "Cambria Math"
                                                         "Gentium Plus"                 ; 30/32
                                                         "Charis SIL"                   ; 30/32
                                                         "Doulos SIL"                   ; 30/32
                                                         "Code2000"
                                                         "Quivira"                      ; 32/32  @4.1 @3.80000305175781
                                                         "Everson Mono:weight=bold"     ; 32/32
                                                         "ALPHABETUM Unicode"           ; 32/32
                                                         ))
    ("Latin Extended-D"                                 (
                                                         "FreeMono"                     ;   5/159  ; this will give poor results if existence-checks is set to 'first
                                                         "DejaVu Sans Mono"             ;  17/159  @2.35 @2.34999084472656
                                                         "DejaVu Sans:width=condensed"  ;  62/159
                                                         "Charis SIL"                   ;  38/159
                                                         "Doulos SIL"                   ;  38/159
                                                         "Junicode"                     ;  97/159
                                                         "Cardo"                        ;  93/159
                                                         "Quivira"                      ; 152/159  @4.1 @3.80000305175781
                                                         "Code2000"                     ; 114/159
                                                         "Everson Mono:weight=bold"     ; 152/159
                                                         "ALPHABETUM Unicode"           ;  94/159
                                                         ))
    ("Latin Extended-E"                                 (
                                                         "Quivira"                      ; 50/54  @4.1 @3.80000305175781
                                                         "Everson Mono:weight=bold"     ; 48/54
                                                         "HanaMinA"                     ; 29/54
                                                         ))
    ;; ("Latin-1 Supplement"                            (                               ; hopefully well-covered by the default font
    ;;                                                   "Monaco"
    ;;                                                   "Consolas"
    ;;                                                   "DejaVu Sans Mono"
    ;;                                                   "Courier New"
    ;;                                                   "DejaVu Sans:width=condensed"
    ;;                                                   "Quivira"
    ;;                                                  ))
    ("Lepcha"                                           (
                                                         "Mingzat"                      ; 74/74
                                                         "Noto Sans Lepcha"             ; 74/74
                                                         ))
    ("Letterlike Symbols"                               (
                                                         "Monaco"                       ; 26/80
                                                         "Noto Sans Symbols"            ; 80/80
                                                         "Segoe UI Symbol"              ; 80/80
                                                         "Apple Symbols"                ; 77/80
                                                         "Cambria Math"                 ; 68/80
                                                         "DejaVu Sans:width=condensed"  ; 75/80
                                                         "Arial Unicode MS"             ; 57/80
                                                         "Code2000"                     ; 80/80
                                                         "Symbola"                      ; 80/80  @8.00 @8
                                                         "Quivira"                      ; 80/80  @4.1 @3.80000305175781
                                                         "HAN NOM A"                    ; 56/80
                                                         "Everson Mono:weight=bold"     ; 80/80
                                                         ))
    ("Limbu"                                            (
                                                         "Noto Sans Limbu"              ; 66/68
                                                         "Namdhinggo SIL"               ; 66/68
                                                         "MPH 2B Damase"
                                                         "Code2000"
                                                         ))
    ("Linear A"                                         (
                                                         "Aegean"                       ; 341/341
                                                         ))
    ("Linear B Ideograms"                               (
                                                         "Noto Sans Linear B"           ; 123/123
                                                         "Aegean"                       ; 123/123
                                                         "Code2001"                     ; 123/123
                                                         "Everson Mono:weight=bold"     ; 123/123
                                                         "ALPHABETUM Unicode"           ; 123/123
                                                         "MPH 2B Damase"                ;  73/123
                                                         ))
    ("Linear B Syllabary"                               (
                                                         "Noto Sans Linear B"           ; 88/88
                                                         "Aegean"                       ; 88/88
                                                         "Code2001"                     ; 88/88
                                                         "Everson Mono:weight=bold"     ; 88/88
                                                         "ALPHABETUM Unicode"           ; 88/88
                                                         "MPH 2B Damase"                ; 74/88
                                                         "Penuturesu"                   ; 73/88
                                                         ))
    ("Lisu"                                             (
                                                         "Lisu Unicode"                 ; 48/48
                                                         "Miao Unicode"                 ; 48/48
                                                         "Noto Sans Lisu"               ; 48/48
                                                         "Lisu Tzimu"                   ; 48/48
                                                         "Quivira"                      ; 48/48  @4.1 @3.80000305175781
                                                         "Everson Mono:weight=bold"     ; 48/48
                                                         ))
    ;; ("Low Surrogates"                                (""))                           ; no displayable characters
    ("Lycian"                                           (
                                                         "Segoe UI Historic"            ; 29/29  @1.00 @1
                                                         "Noto Sans Lycian"             ; 29/29
                                                         "Aegean"                       ; 29/29
                                                         "Quivira"                      ; 29/29  @4.1 @3.80000305175781
                                                         "Everson Mono:weight=bold"     ; 29/29
                                                         "ALPHABETUM Unicode"           ; 29/29
                                                         ))
    ("Lydian"                                           (
                                                         "Segoe UI Historic"            ; 27/27  @1.00 @1
                                                         "Noto Sans Lydian"             ; 27/27
                                                         "Aegean"                       ; 27/27
                                                         "Quivira"                      ; 27/27  @4.1 @3.80000305175781
                                                         "Everson Mono:weight=bold"     ; 27/27
                                                         "ALPHABETUM Unicode"           ; 27/27
                                                         ))
    ;; ("Mahajani"                                      (""))                           ; todo added in Unicode 7.0
    ("Mahjong Tiles"                                    (
                                                         "Segoe UI Symbol"              ; 44/44
                                                         "Symbola"                      ; 44/44  @8.00 @8
                                                         "Noto Sans Symbols"            ; 44/44
                                                         "Quivira"                      ; 44/44  @4.1 @3.80000305175781
                                                         "Everson Mono"                 ; 44/44
                                                         ))
    ("Malayalam"                                        (
                                                         "Malayalam Sangam MN"
                                                         "Nirmala UI"                   ; 98/100
                                                         "Kartika"                      ; 98/100
                                                         "Code2000"                     ; 95/100
                                                         "Akshar Unicode"               ; 83/100
                                                         "Samyak Malayalam"             ; 68/100
                                                         "Samyak"                       ; 66/100
                                                         "Arial Unicode MS"             ; 78/100
                                                         ))
    ("Mandaic"                                          (
                                                         "Noto Sans Mandaic"            ; 29/29
                                                         ))
    ;; ("Manichaean"                                    (""))                           ; todo added in Unicode 7.0
    ("Mathematical Alphanumeric Symbols"                (
                                                         "Cambria Math"                 ; 994/996
                                                         "Noto Sans Symbols"            ; 996/996
                                                         "Asana Math"                   ; 996/996
                                                         "Code2001"                     ; 994/996
                                                         "Symbola"                      ; 996/996  @8.00 @8
                                                         "Quivira"                      ; 996/996  @4.1 @3.80000305175781
                                                         "Everson Mono:weight=bold"     ; 992/996
                                                         ))
    ("Mathematical Operators"                           (
                                                         "Monaco"                       ;  47/256
                                                         "DejaVu Sans Mono"             ; 178/256  @2.35 @2.34999084472656
                                                         "Segoe UI Symbol"              ; 256/256
                                                         "Cambria Math"                 ; 256/256
                                                         "DejaVu Sans:width=condensed"  ; 256/256
                                                         "Noto Sans Symbols"            ; 256/256
                                                         "Apple Symbols"                ; 256/256
                                                         "Asana Math"                   ; 256/256
                                                         "Arial Unicode MS"             ; 242/256
                                                         "Code2000"                     ; 256/256
                                                         "Symbola"                      ; 256/256  @8.00 @8
                                                         "Quivira"                      ; 256/256  @4.1 @3.80000305175781
                                                         "Everson Mono:weight=bold"     ; 256/256
                                                         "FreeMono"                     ; 242/256
                                                         ))
    ("Meetei Mayek"                                     (
                                                         "Noto Sans Meetei Mayek"       ; 56/56
                                                         "Eeyek Unicode"                ; 56/56  fails X11, OS X
                                                         "Meetei Mayek"                 ; 56/56  "Eeyek Unicode" appears under this name in OS X
                                                         ))
    ("Meetei Mayek Extensions"                          (
                                                         "Noto Sans Meetei Mayek"       ; 23/23
                                                         ))
    ;; ("Mende Kikakui"                                 (""))                           ; todo added in Unicode 7.0
    ("Meroitic Cursive"                                 (
                                                         "Nilus"                        ; 90/90
                                                         "Segoe UI Historic"            ; 26/90  @1.00 @1
                                                         "Segoe UI Symbol"              ; 26/90
                                                         ))
    ("Meroitic Hieroglyphs"                             (
                                                         "Nilus"                        ; 32/32
                                                         ))
    ("Miao"                                             (
                                                         "Miao Unicode"                 ; 133/133
                                                         "Albanian"                     ; 106/133
                                                         ))
    ("Miscellaneous Mathematical Symbols-A"             (
                                                         "Noto Sans Symbols"            ; 48/48
                                                         "Apple Symbols"                ; 25/48
                                                         "Segoe UI Symbol"              ; 46/48
                                                         "Asana Math"                   ; 48/48
                                                         "Code2000"
                                                         "Symbola"                      ; 48/48  @8.00 @8
                                                         "Quivira"                      ; 48/48  @4.1 @3.80000305175781
                                                         "Cambria Math"                 ; 28/48
                                                         "Everson Mono:weight=bold"     ; 48/48
                                                         ))
    ("Miscellaneous Mathematical Symbols-B"             (
                                                         "Noto Sans Symbols"            ; 128/128
                                                         "Segoe UI Symbol"              ; 128/128
                                                         "Apple Symbols"                ; 128/128
                                                         "Cambria Math"                 ; 128/128
                                                         "Asana Math"                   ; 128/128
                                                         "Code2000"                     ; 128/128
                                                         "Symbola"                      ; 128/128  @8.00 @8
                                                         "Quivira"                      ; 128/128  @4.1 @3.80000305175781
                                                         ))
    ("Miscellaneous Symbols"                            (
                                                         "Noto Sans Symbols"            ; 256/256
                                                         "Segoe UI Symbol"              ; 256/256
                                                         "Apple Symbols"                ; 256/256
                                                         "DejaVu Sans Mono"             ; 149/256  @2.35 @2.34999084472656
                                                         "DejaVu Sans:width=condensed"  ; 187/256
                                                         "Arial Unicode MS"             ; 106/256
                                                         "Symbola"                      ; 256/256  @8.00 @8
                                                         "Quivira"                      ; 220/256  @4.1 @3.80000305175781
                                                         "MS Reference Sans Serif"      ;  33/256
                                                         "Cardo"                        ;  31/256
                                                         "Code2000"                     ; 183/256
                                                         "Everson Mono:weight=bold"     ; 191/256
                                                         ))
    ("Miscellaneous Symbols and Arrows"                 (
                                                         "Symbola"                      ; 206/206  @8.00 @8
                                                         "Quivira"                      ; 202/206  @4.1 @3.80000305175781
                                                         "Asana Math"                   ;  53/206
                                                         "Code2000"                     ;  82/206
                                                         "Segoe UI Symbol"              ;  87/206
                                                         "Noto Sans Symbols"            ;  87/206
                                                         ))
    ("Miscellaneous Symbols and Pictographs"            (
                                                         "Apple Color Emoji"            ; 533/766
                                                         "Segoe UI Symbol"              ; 529/766
                                                         "Symbola"                      ; 766/766  @8.00 @8
                                                         "Quivira"                      ; 191/766  @4.1 @3.80000305175781
                                                         ))
    ("Miscellaneous Technical"                          (
                                                         "Segoe UI Symbol"              ; 244/251
                                                         "Noto Sans Symbols"            ; 244/251
                                                         "Apple Symbols"                ; 232/251
                                                         "Cambria Math"                 ; 208/251
                                                         "DejaVu Sans Mono"             ; 136/251  @2.35 @2.34999084472656
                                                         "Code2000"                     ; 228/251
                                                         "Symbola"                      ; 251/251  @8.00 @8
                                                         "Quivira"                      ; 251/251  @4.1 @3.80000305175781
                                                         "Everson Mono:weight=bold"     ; 244/251
                                                         ))
    ("Modi"                                             (
                                                         "MarathiCursiveG"              ; 79/79
                                                         ))
    ("Modifier Tone Letters"                            (
                                                         "Apple Symbols"                ; 27/32
                                                         "Noto Sans Symbols"            ; 32/32
                                                         "Gentium Plus"                 ; 32/32
                                                         "Code2000"                     ; 32/32
                                                         "Quivira"                      ; 32/32  @4.1 @3.80000305175781
                                                         "Charis SIL"                   ; 32/32
                                                         "Doulos SIL"                   ; 32/32
                                                         "DejaVu Sans Mono"             ; 20/32  @2.35 @2.34999084472656
                                                         ))
    ("Mongolian"                                        (
                                                         "STFangsong"
                                                         "STHeiti"
                                                         "STKaiti"
                                                         "STSong"
                                                         "Noto Sans Mongolian"          ; 156/156
                                                         "Mongolian Baiti"
                                                         "Daicing Xiaokai"
                                                         "Code2000"
                                                         ))
    ("Mro"                                              (
                                                         "Mro Unicode"                  ; 43/43
                                                         ))
    ;; ("Multani"                                       (""))                           ; todo added in Unicode 8.0
    ("Musical Symbols"                                  (
                                                         "Noto Sans Symbols"            ; 220/231
                                                         "Musica"                       ; 220/231 - OS X rendering issue? see "Musical Symbol G Clef"
                                                         "FreeSerif"                    ; 220/231
                                                         "Symbola"                      ; 231/231  @8.00 @8 - OS X rendering issue? see "Musical Symbol G Clef"
                                                         "Quivira"                      ;  92/231  @4.1 @3.80000305175781
                                                         ))
    ("Myanmar"                                          (
                                                         "Noto Sans Myanmar"            ; 160/160
                                                         "Noto Sans Myanmar UI"         ; 160/160
                                                         "Myanmar Text"                 ; 160/160
                                                         "Myanmar Sangam MN"            ;  85/160
                                                         "Myanmar MN"                   ;  85/160
                                                         "TharLon"                      ; 160/160
                                                         "Yunghkio"                     ; 160/160
                                                         "Myanmar3"                     ;  85/160
                                                         "Masterpiece Uni Sans"         ;  90/160
                                                         "Padauk"                       ; 160/160
                                                         "Code2000"                     ; 156/160
                                                         "Tai Le Valentinium"           ;  10/160
                                                         ))
    ("Myanmar Extended-A"                               (
                                                         "Noto Sans Myanmar"            ; 28/32
                                                         "Noto Sans Myanmar UI"         ; 28/32
                                                         "Myanmar Text"                 ; 28/32
                                                         "Padauk"                       ; 28/32
                                                         "TharLon"                      ; 28/32
                                                         "Yunghkio"                     ; 28/32
                                                         ))
    ("Myanmar Extended-B"                               (                               ; todo very poor coverage
                                                         "TharLon"                      ; 7/31
                                                         "Yunghkio"                     ; 7/31
                                                         ))
    ("Nabataean"                                        (                               ; todo free alternative
                                                         "Everson Mono:weight=bold"     ; 40/40
                                                         ))
    ("New Tai Lue"                                      (
                                                         "Noto Sans New Tai Lue"        ; 83/83
                                                         "Microsoft New Tai Lue"
                                                         "Dai Banna SIL Book"
                                                         "Dai Banna SIL Book:style=Regular"
                                                         ))
    ("NKo"                                              (
                                                         "Ebrima"
                                                         "Conakry"                      ; 59/59
                                                         "DejaVu Sans:width=condensed"  ; 54/59
                                                         "Noto Sans NKo"                ; 59/59
                                                         "Code2000"                     ; 59/59
                                                         ))
    ("Number Forms"                                     (
                                                         "DejaVu Sans:width=condensed"  ; 55/60
                                                         "Asana Math"                   ; 45/60
                                                         "Arial Unicode MS"             ; 48/60
                                                         "Junicode"                     ; 58/60
                                                         "Symbola"                      ; 60/60  @8.00 @8
                                                         "Quivira"                      ; 58/60  @4.1 @3.80000305175781
                                                         "Charis SIL"                   ; 54/60
                                                         "Doulos SIL"                   ; 54/60
                                                         "Code2000"                     ; 54/60
                                                         "Everson Mono:weight=bold"     ; 58/60
                                                         "FreeMono"                     ; 45/60
                                                         "ALPHABETUM Unicode"           ; 58/60
                                                         ))
    ("Ogham"                                            (
                                                         "Segoe UI Historic"            ; 29/29  @1.00 @1
                                                         "Segoe UI Symbol"              ; 29/29
                                                         "Noto Sans Ogham"              ; 29/29
                                                         "DejaVu Sans:width=condensed"
                                                         "BabelStone Modern"            ; 29/29
                                                         "Code2000"
                                                         "Aboriginal Serif"             ; 29/29
                                                         "Quivira"                      ; 29/29  @4.1 @3.80000305175781
                                                         "Everson Mono:weight=bold"     ; 29/29
                                                         "ALPHABETUM Unicode"           ; 29/29
                                                         ))
    ("Ol Chiki"                                         (
                                                         "Nirmala UI"                   ; 48/48
                                                         "Noto Sans Ol Chiki"           ; 48/48
                                                         "Code2000"                     ; 48/48
                                                         ))
    ("Old Hungarian"                                    (
                                                         "OldHungarian"                 ; 108/108
                                                         ))
    ("Old Italic"                                       (
                                                         "Segoe UI Historic"            ; 35/36  @1.00 @1
                                                         "Segoe UI Symbol"              ; 35/36
                                                         "DejaVu Sans:width=condensed"  ; 35/36
                                                         "Cardo"                        ; 35/36
                                                         "New Athena Unicode"           ; 35/36
                                                         "Aegean"                       ; 36/36
                                                         "Noto Sans Old Italic"         ; 35/36, characters are RTL
                                                         "Albanian"                     ; 36/36
                                                         "Code2001"                     ; 35/36, characters are RTL
                                                         "Quivira"                      ; 36/36  @4.1 @3.80000305175781
                                                         "Everson Mono:weight=bold"     ; 35/36
                                                         "FreeMono"                     ; 35/36
                                                         "ALPHABETUM Unicode"           ; 35/36
                                                         ))
    ("Old North Arabian"                                (                               ; todo free alternative
                                                         "Marib"
                                                         ))
    ("Old Permic"                                       (                               ; todo free alternative
                                                         "Everson Mono:weight=bold"     ; 40/40
                                                         ))
    ("Old Persian"                                      (
                                                         "Segoe UI Historic"            ; 50/50  @1.00 @1
                                                         "Noto Sans Old Persian"        ; 50/50
                                                         "MPH 2B Damase"                ; 50/50
                                                         "Aegean"                       ; 50/50
                                                         "Code2001"                     ; 50/50
                                                         "FreeSans"                     ; 50/50
                                                         "ALPHABETUM Unicode"           ; 50/50
                                                         ))
    ("Old South Arabian"                                (
                                                         "Segoe UI Historic"            ; 32/32  @1.00 @1
                                                         "Noto Sans Old South Arabian"  ; 32/32
                                                         "Quivira"                      ; 32/32  @4.1 @3.80000305175781
                                                         "Qataban"                      ; 32/32
                                                         "Everson Mono:weight=bold"     ; 32/32
                                                         ))
    ("Old Turkic"                                       (
                                                         "Noto Sans Old Turkic"         ; 73/73
                                                         "Segoe UI Historic"            ; 73/73  @1.00 @1
                                                         "Segoe UI Symbol"              ; 73/73
                                                         "Quivira"                      ; 73/73  @4.1 @3.80000305175781
                                                         "Everson Mono:weight=bold"     ; 73/73
                                                         ))
    ("Optical Character Recognition"                    (
                                                         "Apple Symbols"                ; 11/11
                                                         "Segoe UI Symbol"              ; 11/11
                                                         "Noto Sans Symbols"            ; 11/11
                                                         "Arial Unicode MS"
                                                         "Symbola"                      ; 11/11  @8.00 @8
                                                         "Quivira"                      ; 11/11  @4.1 @3.80000305175781
                                                         "FreeMono"                     ; 11/11
                                                         "BabelStone Modern"            ; 11/11
                                                         "Code2000"
                                                         "Everson Mono"                 ; 11/11
                                                         ))
    ("Oriya"                                            (
                                                         "Noto Sans Oriya"              ; 90/90
                                                         "Oriya Sangam MN"
                                                         "Nirmala UI"                   ; 84/90
                                                         "Kalinga"                      ; 84/90
                                                         "Samyak Oriya"                 ; 82/90
                                                         "Samyak"                       ; 82/90
                                                         "Code2000"                     ; 84/90
                                                         "Arial Unicode MS"             ; 79/90
                                                         ))
    ("Ornamental Dingbats"                              (
                                                         "Symbola"                      ; 48/48  @8.00 @8
                                                         ))
    ("Osmanya"                                          (
                                                         "Noto Sans Osmanya"            ; 40/40
                                                         "Ebrima"                       ; 40/40
                                                         "Andagii"                      ; 40/40
                                                         "MPH 2B Damase"                ; 40/40
                                                         "Code2001"                     ; 40/40
                                                         "Everson Mono:weight=bold"     ; 40/40
                                                         ))
    ;; ("Pahawh Hmong"                                  (""))                           ; todo added in Unicode 7.0
    ;; ("Palmyrene"                                     (""))                           ; todo added in Unicode 7.0
    ;; ("Pau Cin Hau"                                   (""))                           ; todo added in Unicode 7.0
    ("Phags-pa"                                         (
                                                         "BabelStone Phags-pa Book"     ; 56/56
                                                         "BabelStone Phags-pa Book:style=Regular"
                                                         "Noto Sans Phags-pa"           ; 56/56
                                                         "Microsoft PhagsPa"
                                                         "Code2000"                     ; 56/56
                                                         ))
    ("Phaistos Disc"                                    (
                                                         "Aegean"                       ; 46/46
                                                         "Noto Sans Symbols"            ; 46/46
                                                         "Symbola"                      ; 46/46  @8.00 @8
                                                         "Everson Mono:weight=bold"     ; 46/46
                                                         "Code2001"                     ; 46/46
                                                         "ALPHABETUM Unicode"           ; 46/46
                                                         ))
    ("Phoenician"                                       (
                                                         "Segoe UI Historic"            ; 29/29  @1.00 @1
                                                         "Noto Sans Phoenician"         ; 29/29
                                                         "Aegean"                       ; 29/29
                                                         "Quivira"                      ; 29/29  @4.1 @3.80000305175781
                                                         "Code2001"                     ; 27/29
                                                         "Everson Mono:weight=bold"     ; 29/29
                                                         "ALPHABETUM Unicode"           ; 29/29
                                                         ))
    ("Phonetic Extensions"                              (
                                                         "Monaco"                       ; 109/128
                                                         "Consolas"                     ; 128/128
                                                         "Calibri"                      ; 128/128
                                                         "Noto Sans"                    ; 128/128
                                                         "Aboriginal Sans"              ;  79/128
                                                         "Charis SIL"                   ; 128/128
                                                         "Doulos SIL"                   ; 128/128
                                                         "Quivira"                      ; 128/128  @4.1 @3.80000305175781
                                                         "Courier New"                  ; 128/128
                                                         "DejaVu Sans:width=condensed"
                                                         "Code2000"
                                                         "Everson Mono:weight=bold"     ; 128/128
                                                         "ALPHABETUM Unicode"           ; 128/128
                                                         ))
    ("Phonetic Extensions Supplement"                   (
                                                         "Consolas"                     ; 64/64
                                                         "Calibri"                      ; 64/64
                                                         "Courier New"                  ; 64/64 ; todo a better OS X choice
                                                         "Noto Sans"                    ; 64/64
                                                         "Aboriginal Sans"              ; 35/64
                                                         "Charis SIL"                   ; 64/64
                                                         "Doulos SIL"                   ; 64/64
                                                         "Quivira"                      ; 64/64  @4.1 @3.80000305175781
                                                         "DejaVu Sans Mono"             ; 37/64  @2.35 @2.34999084472656
                                                         "DejaVu Sans:width=condensed"  ; 38/64
                                                         "Code2000"                     ; 64/64
                                                         "Everson Mono:weight=bold"     ; 64/64
                                                         "ALPHABETUM Unicode"           ; 64/64
                                                         ))
    ("Playing Cards"                                    (
                                                         "DejaVu Sans:width=condensed"  ; 59/82
                                                         "Symbola"                      ; 82/82  @8.00 @8
                                                         "Noto Sans Symbols"            ; 59/82
                                                         "Segoe UI Symbol"              ; 59/82
                                                         "Quivira"                      ; 82/82  @4.1 @3.80000305175781
                                                         ))
    ;; ("Private Use Area"                              (
    ;;                                                   "ALPHABETUM Unicode"           ; 2,405/6,400 MUFI
    ;;                                                   "UnBatang"                     ; 2,048/6,400
    ;;                                                   "Jomolhari"                    ; 1,537/6,400
    ;;                                                   "Code2000"                     ; 1,373/6,400 conflicts MUFI
    ;;                                                   "BabelStone Han"               ; 1,346/6,400
    ;;                                                   "Siddhanta"                    ; 1,292/6,400
    ;;                                                   "Cardo"                        ; 1,209/6,400 MUFI
    ;;                                                   "Unidings"                     ; 1,024/6,400 conflicts MUFI
    ;;                                                   "Quivira"                      ;   894/6,400  @4.1 @3.80000305175781
    ;;                                                   "Junicode"                     ;   841/6,400 MUFI
    ;;                                                   "Code2001"                     ;   362/6,400
    ;;                                                   "MS Reference Sans Serif"      ;   312/6,400
    ;;                                                   "Doulos SIL"                   ;   229/6,400
    ;;                                                   "Webdings"                     ;   223/6,400
    ;;                                                   "Wingdings"                    ;   223/6,400
    ;;                                                   "Wingdings 2"                  ;   217/6,400
    ;;                                                   "Wingdings 3"                  ;   208/6,400
    ;;                                                   "Symbol"                       ;   188/6,400
    ;;                                                   "MS Reference Specialty"       ;   170/6,400
    ;;                                                   "Abyssinica SIL"               ;   137/6,400
    ;;                                                   "Bookshelf Symbol 7"           ;   111/6,400
    ;;                                                   "Lanna Alif"                   ;    94/6,400
    ;;                                                   "Aksara Bali"                  ;    53/6,400
    ;;                                                   "MPH 2B Damase"                ;    49/6,400
    ;;                                                   "Arial Unicode MS"             ;    43/6,400
    ;;                                                   "Marlett"                      ;    35/6,400
    ;;                                                   "DejaVu Sans"                  ;    31/6,400
    ;;                                                   "Lucida Console"               ;    23/6,400
    ;;                                                   "Hacen Sudan"                  ;    20/6,400
    ;;                                                   "Lucida Sans Unicode"          ;    16/6,400
    ;;                                                   "Samyak Devanagari"            ;    10/6,400
    ;;                                                   "Khmer Mondulkiri"             ;    10/6,400
    ;;                                                   "Khmer Busra MOE"              ;    10/6,400
    ;;                                                   "Khmer Busra"                  ;    10/6,400
    ;;                                                   "BabelStone Phags-pa Book"     ;     7/6,400
    ;;                                                   "Samyak"                       ;     6/6,400
    ;;                                                   ))
    ;; ("Psalter Pahlavi"                               (""))                           ; todo added in Unicode 7.0
    ("Rejang"                                           (
                                                         "Noto Sans Rejang"             ; 37/37
                                                         "Code2000"                     ; 37/37
                                                         "Everson Mono:weight=bold"     ; 37/37
                                                         ))
    ("Rumi Numeral Symbols"                             (
                                                         "HanaMinA"                     ; 31/31
                                                         ))
    ("Runic"                                            (
                                                         "Noto Sans Runic"              ; 81/89
                                                         "Segoe UI Historic"            ; 81/89  @1.00 @1
                                                         "Segoe UI Symbol"              ; 81/89
                                                         "Aboriginal Serif"             ; 81/89
                                                         "Junicode"                     ; 81/89
                                                         "FreeMono"                     ; 81/89
                                                         "Quivira"                      ; 89/89  @4.1 @3.80000305175781
                                                         "Code2000"
                                                         "Cardo"                        ; 81/89
                                                         "Everson Mono:weight=bold"     ; 89/89
                                                         "ALPHABETUM Unicode"           ; 81/89
                                                         ))
    ("Samaritan"                                        (
                                                         "Noto Sans Samaritan"          ; 61/61
                                                         "Quivira"                      ; 61/61  @4.1 @3.80000305175781
                                                         "Everson Mono:weight=bold"     ; 61/61
                                                         ))
    ("Saurashtra"                                       (
                                                         "Noto Sans Saurashtra"         ; 81/81
                                                         "Code2000"
                                                         "Sourashtra"
                                                         ))
    ("Sharada"                                          (
                                                         "Albanian"                     ; 85/94
                                                         ))
    ("Shavian"                                          (
                                                         "Segoe UI Historic"            ; 48/48  @1.00 @1
                                                         "Noto Sans Shavian"            ; 48/48
                                                         "Andagii"                      ; 48/48
                                                         "MPH 2B Damase"
                                                         "Apple Symbols"                ; 48/48
                                                         "Code2001"                     ; 48/48
                                                         "Everson Mono:weight=bold"     ; 48/48
                                                         ))
    ;; ("Shorthand Format Controls"                     (""))                           ; no displayable characters
    ("Siddham"                                          (
                                                         "MuktamsiddhamG"               ; 92/92
                                                         ))
    ("Sinhala"                                          (
                                                         "Noto Sans Sinhala"            ; 80/90
                                                         "Nirmala UI"                   ; 80/90
                                                         "Iskoola Pota"                 ; 80/90
                                                         "Akshar Unicode"               ; 80/90
                                                         "Sinhala Sangam MN"
                                                         ))
    ;; ("Sinhala Archaic Numbers"                       (""))                           ; todo added in Unicode 7.0
    ("Small Form Variants"                              (
                                                         "Apple Symbols"                ; 26/26
                                                         "Arial Unicode MS"
                                                         "WenQuanYi Zen Hei Mono"       ; 25/26
                                                         "Microsoft YaHei"              ; 26/26
                                                         "Microsoft YaHei UI"           ; 26/26
                                                         "Code2000"
                                                         ))
    ("Sora Sompeng"                                     (                               ; todo free coverage
                                                         "Nirmala UI"                   ; 35/35
                                                         ))
    ;; ("Spacing Modifier Letters"                      (                               ; hopefully well-covered by the default font
    ;;                                                   "Monaco"                       ; 79/80
    ;;                                                   "Consolas"                     ; 80/80
    ;;                                                   "DejaVu Sans Mono"             ; 48/80
    ;;                                                   'Cambria Math"                 ; 80/80
    ;;                                                   "Arial Unicode MS"             ; 57/80
    ;;                                                   "Code2000"                     ; 80/80
    ;;                                                   "DejaVu Sans:width=condensed"  ; 63/80
    ;;                                                   "Quivira"                      ; 80/80
    ;;                                                   "Symbola"                      ; 80/80  @8.00 @8
    ;;                                                   ))
    ("Specials"                                         (
                                                         "BabelStone Modern"            ; 5/5
                                                         "Noto Sans Symbols"            ; 5/5
                                                         "Apple Symbols"                ; 5/5
                                                         "Arial Unicode MS"
                                                         "Symbola"                      ; 5/5  @8.00 @8
                                                         "DejaVu Sans Mono"             ; 5/5  @2.35 @2.34999084472656
                                                         "DejaVu Sans:width=condensed"
                                                         "Quivira"                      ; 5/5  @4.1 @3.80000305175781
                                                         "FreeMono"                     ; 5/5
                                                         "BabelStone Han"               ; 5/5
                                                         ))
    ("Sundanese"                                        (
                                                         "Noto Sans Sundanese"          ; 64/64
                                                         "Sundanese Unicode"            ; 55/64
                                                         ))
    ("Sundanese Supplement"                             (
                                                         "Noto Sans Sundanese"          ; 8/8
                                                         ))
    ("Superscripts and Subscripts"                      (
                                                         "Consolas"                     ; 34/42
                                                         "Monaco"                       ; 29/42
                                                         "Apple Symbols"                ; 29/42
                                                         "Cambria Math"                 ; 34/42
                                                         "DejaVu Sans Mono"             ; 42/42  @2.35 @2.34999084472656
                                                         "DejaVu Sans:width=condensed"  ; 42/42
                                                         "Segoe UI Symbol"              ; 42/42
                                                         "Asana Math"                   ; 34/42
                                                         "Charis SIL"                   ; 34/42
                                                         "Doulos SIL"                   ; 34/42
                                                         "Symbola"                      ; 42/42  @8.00 @8
                                                         "Quivira"                      ; 42/42  @4.1 @3.80000305175781
                                                         "Everson Mono:weight=bold"     ; 42/42
                                                         "FreeMono"                     ; 34/42
                                                         ))
    ("Supplemental Arrows-A"                            (
                                                         "Segoe UI Symbol"              ; 16/16
                                                         "Cambria Math"                 ; 16/16
                                                         "DejaVu Sans:width=condensed"  ; 16/16
                                                         "Asana Math"                   ; 16/16
                                                         "Quivira"                      ; 16/16  @4.1 @3.80000305175781
                                                         "Symbola"                      ; 16/16  @8.00 @8
                                                         "Apple Symbols"                ; 16/16
                                                         "Noto Sans Symbols"            ; 16/16
                                                         "Code2000"                     ; 16/16
                                                         "Everson Mono:weight=bold"     ; 16/16
                                                         "FreeMono"                     ; 10/16
                                                         "BabelStone Modern"            ; 12/16
                                                         ))
    ("Supplemental Arrows-B"                            (
                                                         "Cambria Math"                 ; 128/128
                                                         "Segoe UI Symbol"              ; 128/128
                                                         "Apple Symbols"                ; 128/128
                                                         "Noto Sans Symbols"            ; 128/128
                                                         "Asana Math"                   ; 128/128
                                                         "Quivira"                      ; 128/128  @4.1 @3.80000305175781
                                                         "Symbola"                      ; 128/128  @8.00 @8
                                                         "Code2000"                     ; 128/128
                                                         "Everson Mono:weight=bold"     ; 128/128
                                                         ))
    ("Supplemental Arrows-C"                            (
                                                         "Symbola"                      ; 148/148  @8.00 @8
                                                         ))
    ("Supplemental Mathematical Operators"              (
                                                         "Cambria Math"                 ; 256/256
                                                         "Segoe UI Symbol"              ; 256/256
                                                         "Noto Sans Symbols"            ; 256/256
                                                         "Apple Symbols"                ; 251/256
                                                         "Asana Math"                   ; 256/256
                                                         "Code2000"                     ; 256/256
                                                         "Symbola"                      ; 256/256  @8.00 @8
                                                         "Quivira"                      ; 256/256  @4.1 @3.80000305175781
                                                         "Everson Mono:weight=bold"     ; 195/256
                                                         ))
    ("Supplemental Punctuation"                         (
                                                         "DejaVu Sans Mono"             ;  7/67  @2.35 @2.34999084472656 ; this will give poor results if existence-checks is set to 'first
                                                         "Segoe UI Symbol"              ; 50/67
                                                         "Noto Sans Symbols"            ; 60/67
                                                         "Antinoou"                     ; 46/67
                                                         "New Athena Unicode"           ; 60/67
                                                         "Cardo"                        ; 43/67
                                                         "Aegean"                       ; 48/67
                                                         "Symbola"                      ; 67/67  @8.00 @8
                                                         "Quivira"                      ; 67/67  @4.1 @3.80000305175781
                                                         "Everson Mono:weight=bold"     ; 60/67
                                                         "Code2000"                     ; 49/67
                                                         "ALPHABETUM Unicode"           ; 50/67
                                                         ))
    ("Supplemental Symbols and Pictographs"             (
                                                         "Symbola"                      ; 15/15  @8.00 @8
                                                         ))
    ;; ("Supplementary Private Use Area-A"              (
    ;;                                                   "Aegean"                       ; 3,600/65,534
    ;;                                                   "Aegyptus"                     ; 7,243/65,534
    ;;                                                   "Jomolhari"                    ;   659/65,534
    ;;                                                   "Cardo"                        ;   480/65,534 MUFI
    ;;                                                   "Code2001"                     ;   292/65,534
    ;;                                                   "Symbola"                      ;   166/65,534  @8.00 @8
    ;;                                                   "Analecta"                     ;   102/65,534
    ;;                                                   "Musica"                       ;    43/65,534
    ;;                                                   "Akkadian"                     ;    17/65,534
    ;;                                                   ))
    ;; ("Supplementary Private Use Area-B"              (""))
    ;; ("Sutton SignWriting"                            (""))                           ; todo added in Unicode 8.0
    ("Syloti Nagri"                                     (
                                                         "Noto Sans Syloti Nagri"       ; 44/44
                                                         "MPH 2B Damase"
                                                         ))
    ("Syriac"                                           (
                                                         "Segoe UI Historic"            ; 77/77  @1.00 @1
                                                         "Estrangelo Edessa"            ; 77/77
                                                         "Estrangelo Nisibin"           ; 71/77
                                                         "Code2000"                     ; 50/77
                                                        ))
    ("Tagalog"                                          (
                                                         "Quivira"                      ; 20/20  @4.1 @3.80000305175781
                                                         "Noto Sans Tagalog"            ; 20/20
                                                         ))
    ("Tagbanwa"                                         (
                                                         "Noto Sans Tagbanwa"           ; 18/18
                                                         "Quivira"                      ; 18/18  @4.1 @3.80000305175781
                                                         ))
    ("Tags"                                             (
                                                         "BabelStone Modern"            ; 97/97
                                                         "BabelStone Han"               ; 97/97
                                                         ))
    ("Tai Le"                                           (
                                                         "Microsoft Tai Le"             ; 35/35
                                                         "TharLon"                      ; 35/35
                                                         "Noto Sans Tai Le"             ; 35/35
                                                         "Yunghkio"                     ; 35/35
                                                         "Tai Le Valentinium"           ; 35/35
                                                         "MPH 2B Damase"                ; 35/35
                                                         "FreeSerif"                    ; 35/35
                                                         ))
    ("Tai Tham"                                         (
                                                         "Noto Sans Tai Tham"           ; 127/127
                                                         "Lanna Alif"                   ; 127/127
                                                         "Chiangsaen Alif"              ; 127/127
                                                         "Lanna Unicode UI"             ; 127/127
                                                         "Monlam Uni Sans Serif"        ; 127/127  @3.0 2008 @1.42298889160156
                                                         ))
    ("Tai Viet"                                         (
                                                         "Tai Heritage Pro"             ; 72/72
                                                         "Noto Sans Tai Viet"           ; 72/72
                                                         ))
    ("Tai Xuan Jing Symbols"                            (
                                                         "WenQuanYi Zen Hei Mono"       ; 87/87
                                                         "Apple Symbols"                ; 87/87
                                                         "Noto Sans Symbols"            ; 87/87
                                                         "Segoe UI Symbol"              ; 87/87
                                                         "BabelStone Han"               ; 87/87
                                                         "DejaVu Sans:width=condensed"
                                                         "Symbola"                      ; 87/87  @8.00 @8
                                                         "Quivira"                      ; 87/87  @4.1 @3.80000305175781
                                                         "BabelStone Modern"            ; 87/87
                                                         "Code2001"                     ; 87/87
                                                         "Everson Mono:weight=bold"     ; 87/87
                                                         ))
    ("Takri"                                            (
                                                         "Albanian"                     ; 66/66
                                                         ))
    ("Tamil"                                            (
                                                         "Latha"                        ; 72/72
                                                         "Noto Sans Tamil"              ; 72/72
                                                         "Noto Sans Tamil UI"           ; 72/72
                                                         "Nirmala UI"                   ; 72/72
                                                         "Tamil MN"
                                                         "Tamil Sangam MN"
                                                         "InaiMathi"
                                                         "Vijaya"                       ; 72/72
                                                         "Maduram"
                                                         "Akshar Unicode"               ; 61/72
                                                         "Samyak Tamil"                 ; 49/72
                                                         "Samyak"                       ; 49/72
                                                         "Code2000"                     ; 71/72
                                                         "Arial Unicode MS"             ; 61/72
                                                         ))
    ("Telugu"                                           (
                                                         "Noto Sans Telugu"             ; 93/96
                                                         "Noto Sans Telugu UI"          ; 93/96
                                                         "Telugu Sangam MN"
                                                         "Vani"                         ; 93/96
                                                         "Nirmala UI"                   ; 93/96
                                                         "Gautami"                      ; 93/96
                                                         "Akshar Unicode"               ; 80/96
                                                         "Code2000"                     ; 93/96
                                                         "Arial Unicode MS"             ; 80/96
                                                         ))
    ("Thaana"                                           (
                                                         "MV Boli"                      ; 50/50
                                                         "Noto Sans Thaana"             ; 50/50
                                                         "MPH 2B Damase"                ; 50/50
                                                         "Code2000"                     ; 50/50
                                                         "Everson Mono:weight=bold"     ; 50/50
                                                         ))
    ("Thai"                                             (
                                                         "Thonburi"
                                                         "DokChampa"                    ; 87/87
                                                         "Noto Sans Thai"               ; 87/87
                                                         "Noto Sans Thai UI"            ; 87/87
                                                         "Noto Serif Thai"              ; 87/87
                                                         "Ayuthaya"
                                                         "Silom"
                                                         "Krungthep"
                                                         "Sathu"
                                                         "Angsana New"                  ; 87/87
                                                         "AngsanaUPC"                   ; 87/87
                                                         "Code2000"
                                                         "Tahoma"                       ; 87/87
                                                         "Arial Unicode MS"             ; 87/87
                                                         "Quivira"                      ; 87/87  @4.1 @3.80000305175781
                                                         "Everson Mono:weight=bold"     ; 87/87
                                                         ))
    ("Tibetan"                                          (
                                                         "Noto Sans Tibetan"            ; 211/211  @1.00              @1
                                                         "Kailasa"                      ; 205/211  @7.0d2e4           @3.00799560546875
                                                         "Kokonor"                      ; 205/211  @7.0d1e2           @10.0110015869141
                                                         "Tibetan Machine Uni"          ; 205/211  @1.901_2007        @1.90098571777344
                                                         "Microsoft Himalaya"           ; 197/211  @5.10              @5.10000610351562
                                                         "Jomolhari"                    ; 195/211  @alpha_0.003c_2006 @0.0030059814453125
                                                         "Monlam Uni Sans Serif"        ; 193/211  @3.0_2008          @1.42298889160156
                                                         "Arial Unicode MS"             ; 168/211  @1.01              @1
                                                         ))
    ("Tifinagh"                                         (
                                                         "Noto Sans Tifinagh"           ; 59/59
                                                         "Ebrima"
                                                         "DejaVu Sans:width=condensed"
                                                         "Code2000"
                                                         "Quivira"                      ; 58/59  @4.1 @3.80000305175781
                                                         "Everson Mono:weight=bold"     ; 59/59
                                                         ))
    ;; ("Tirhuta"                                       (""))                           ; todo added in Unicode 7.0
    ("Transport and Map Symbols"                        (
                                                         "Apple Color Emoji"
                                                         "Segoe UI Symbol"              ; 70/98
                                                         "Symbola"                      ; 98/98  @8.00 @8
                                                         ))
    ("Ugaritic"                                         (
                                                         "Segoe UI Historic"            ; 31/31  @1.00 @1
                                                         "Noto Sans Ugaritic"           ; 31/31
                                                         "Aegean"                       ; 31/31
                                                         "Code2001"                     ; 31/31
                                                         "Andagii"                      ; 31/31
                                                         "Quivira"                      ; 31/31  @4.1 @3.80000305175781
                                                         "Everson Mono:weight=bold"     ; 31/31
                                                         "FreeSans"                     ; 31/31
                                                         "ALPHABETUM Unicode"           ; 31/31
                                                         ))
    ("Unified Canadian Aboriginal Syllabics"            (
                                                         "Aboriginal Sans"              ; 640/640
                                                         "Aboriginal Serif"             ; 640/640
                                                         "Noto Sans Canadian Aboriginal"; 640/640
                                                         "Gadugi"
                                                         "Euphemia UCAS"
                                                         "Euphemia"
                                                         "Code2000"
                                                         "Quivira"                      ; 640/640  @4.1 @3.80000305175781
                                                         "Everson Mono:weight=bold"     ; 640/640
                                                         ))
    ("Unified Canadian Aboriginal Syllabics Extended"   (
                                                         "Aboriginal Sans"              ; 70/70
                                                         "Aboriginal Serif"             ; 70/70
                                                         "Noto Sans Canadian Aboriginal"; 70/70
                                                         "Gadugi"
                                                         "Euphemia UCAS"
                                                         "Euphemia"
                                                         "Quivira"                      ; 70/70  @4.1 @3.80000305175781
                                                         "Everson Mono:weight=bold"     ; 70/70
                                                         ))
    ("Vai"                                              (
                                                         "Ebrima"                       ; 300/300
                                                         "Noto Sans Vai"                ; 300/300
                                                         "Dukor"                        ; 300/300
                                                         "Wakor"                        ; 300/300
                                                         "Code2000"                     ; 300/300
                                                         "Quivira"                      ; 300/300  @4.1 @3.80000305175781
                                                         ))
    ("Variation Selectors"                              (
                                                         "BabelStone Modern"            ; 16/16
                                                         "BabelStone Han"               ; 16/16
                                                         "Code2000"
                                                         ))
    ("Variation Selectors Supplement"                   (
                                                         "BabelStone Modern"            ; 240/240
                                                         "BabelStone Han"               ; 240/240
                                                         ))
    ("Vedic Extensions"                                 (
                                                         "Siddhanta"                    ; 37/41
                                                         ))
    ("Vertical Forms"                                   (
                                                         "Microsoft YaHei"              ; 10/10
                                                         "Microsoft YaHei UI"           ; 10/10
                                                         "Symbola"                      ; 10/10  @8.00 @8
                                                         ))
    ;; ("Warang Citi"                                   (""))                           ; todo added in Unicode 7.0
    ("Yi Radicals"                                      (
                                                         "Noto Sans Yi"                 ; 55/55
                                                         "Nuosu SIL"                    ; 55/55
                                                         "Microsoft Yi Baiti"           ; 55/55
                                                         "STFangsong"                   ; 32/55
                                                         "Code2000"                     ; 55/55
                                                         ))
    ("Yi Syllables"                                     (
                                                         "Noto Sans Yi"                 ; 1,165/1,165
                                                         "Nuosu SIL"                    ; 1,165/1,165
                                                         "Microsoft Yi Baiti"           ; 1,165/1,165
                                                         "STFangsong"                   ; 1,024/1,165
                                                         "Code2000"                     ; 1,165/1,165
                                                         ))
    ("Yijing Hexagram Symbols"                          (
                                                         "WenQuanYi Zen Hei Mono"       ; 64/64
                                                         "Noto Sans Symbols"            ; 64/64
                                                         "Segoe UI Symbol"              ; 64/64
                                                         "Apple Symbols"                ; 64/64
                                                         "DejaVu Sans:width=condensed"
                                                         "BabelStone Han"               ; 64/64
                                                         "Symbola"                      ; 64/64  @8.00 @8
                                                         "Quivira"                      ; 64/64  @4.1 @3.80000305175781
                                                         "BabelStone Modern"            ; 64/64
                                                         "Code2000"
                                                         "Everson Mono:weight=bold"     ; 64/64
                                                         )))
  "Preferred fonts for each Unicode block.

These mappings are only installed in Emacs if a preferred font
for the block is available on your system.  When multiple fonts
are given, each is tried in order."
  :type '(alist :key-type string :value-type (group (repeat :tag "Fonts" (string :tag ""))))
  :options (mapcar 'car unicode-fonts-blocks)
  :group 'unicode-fonts)

(defcustom unicode-fonts-ignore-overrides nil
  "Ignore settings in `unicode-fonts-overrides-mapping'."
  :type 'boolean
  :group 'unicode-fonts)

(defcustom unicode-fonts-overrides-mapping
  '(
    ;; Control Pictures block
    ("Symbol for Escape"                              "Symbol for Escape"                           ("Keyboard"                          ))  ; OS X shift key

    ;; Arrows block
    ("Rightwards Arrow with Hook"                     "Rightwards Arrow with Hook"                  ("Keyboard"                          ))
    ("Rightwards Arrow to Bar"                        "Rightwards Arrow to Bar"                     ("Lucida Grande"                     ))  ; Tab key
    ("Upwards White Arrow"                            "Upwards White Arrow"                         ("Keyboard" "Lucida Grande"          ))  ; OS X shift key
    ("Upwards White Arrow from Bar"                   "Upwards White Arrow from Bar"                ("Keyboard" "Lucida Grande"          ))

    ;; Miscellaneous Technical block
    ("Up Arrowhead"                                   "Up Arrowhead"                                ("Keyboard" "Lucida Grande"          ))  ; OS X control key
    ("Projective"                                     "Projective"                                  ("Lucida Grande"                     ))  ; OS X key?
    ("Up Arrowhead Between Two Horizontal Bars"       "Up Arrowhead Between Two Horizontal Bars"    ("Keyboard" "Lucida Grande"          ))
    ("Place of Interest Sign"                         "Place of Interest Sign"                      ("Keyboard" "Lucida Grande"          ))  ; OS X command key
    ("Option Key"                                     "Option Key"                                  ("Keyboard" "Lucida Grande"          ))
    ("Erase to the Right"                             "Erase to the Right"                          ("Keyboard" "Lucida Grande"          ))
    ("X in a Rectangle Box"                           "X in a Rectangle Box"                        ("Keyboard" "Lucida Grande"          ))
    ("Erase To the Left"                              "Erase To the Left"                           ("Keyboard" "Lucida Grande"          ))  ; Backspace
    ("APL Functional Symbol Quad Backslash"           "APL Functional Symbol Quad Backslash"        ("Lucida Grande"                     ))  ; OS X key?
    ("Alternative Key Symbol"                         "Alternative Key Symbol"                      ("Keyboard" "Lucida Grande"          ))  ; OS X alt key
    ("Broken Circle with Northwest Arrow"             "Broken Circle with Northwest Arrow"          ("Keyboard" "Lucida Grande"          ))  ; OS X power key
    ("Eject Symbol"                                   "Eject Symbol"                                ("Keyboard" "Lucida Grande"          ))
    ("Left Parenthesis Upper Hook"                    "Radical Symbol Bottom"                       ("Cambria Math" "Quivira" "Code2000" ))
    ("Top Half Integral"                              "Bottom Half Integral"                        ("Cambria Math" "Quivira" "Code2000" ))

    ;; General Punctuation block
    ("En Quad"                                        "Zero Width Joiner"                           ("DejaVu Sans" "Symbola" "Arial Unicode MS"))   ; space variations are proportional
    ("Bullet"                                         "Bullet"                                      ("DejaVu Sans:width=condensed"       ))

    ;; Geometric Shapes block
    ("White Bullet"                                   "White Bullet"                                ("DejaVu Sans:width=condensed"       ))

    ;; Mathematical Operators block
    ("Circled Times"                                  "Circled Times"                               ("Arial Unicode MS"                  ))

    ;; Currency Symbols block
    ("Drachma Sign"                                   "Drachma Sign"                                ("DejaVu Sans Mono"                       ))
    ("German Penny Sign"                              "German Penny Sign"                           ("DejaVu Sans Mono"                       ))
    ("New Sheqel Sign"                                "New Sheqel Sign"                             ("DejaVu Sans Mono"                       ))
    ("Livre Tournois Sign"                            "Spesmilo Sign"                               ("Noto Sans Symbols" "Symbola"            ))
    ("Turkish Lira Sign"                              "Turkish Lira Sign"                           ("Noto Sans Symbols" "Symbola" "Noto Sans"))
    ("Nordic Mark Sign"                               "Nordic Mark Sign"                            ("Symbola"                                ))
    ("Manat Sign"                                     "Lari Sign"                                   ("Noto Sans Symbols" "Symbola"            ))
    (#x20BF                                           #x20CF                                        ("Symbola"                                ))

    ;; Dingbats block
    ("White Heavy Check Mark"                         "White Heavy Check Mark"                          ("Symbola"                      ))
    ("Raised Fist"                                    "Raised Hand"                                     ("Symbola"                      ))
    ("Sparkles"                                       "Sparkles"                                        ("Symbola"                      ))
    ("Cross Mark"                                     "Cross Mark"                                      ("Symbola"                      ))
    ("Negative Squared Cross Mark"                    "Negative Squared Cross Mark"                     ("Symbola"                      ))
    ("Black Question Mark Ornament"                   "White Exclamation Mark Ornament"                 ("Symbola"                      ))
    ("Heavy Exclamation Mark Symbol"                  "Heavy Exclamation Mark Symbol"                   ("Symbola"                      ))
    ("Heavy Low Single Comma Quotation Mark Ornament" "Heavy Low Double Comma Quotation Mark Ornament"  ("Symbola"                      ))
    ("Dingbat Negative Circled Digit One"             "Dingbat Negative Circled Sans-Serif Number Ten"  ("Zapf Dingbats" "DejaVu Sans:width=condensed" "Symbola"))
    ("Heavy Plus Sign"                                "Heavy Division Sign"                             ("Symbola"                      ))
    ("Curly Loop"                                     "Curly Loop"                                      ("Symbola"                      ))
    ("Double Curly Loop"                              "Double Curly Loop"                               ("Symbola"                      ))

    ;; Phonetic Extensions block
    ("Latin Small Letter Turned A"                    "Latin Small Letter Turned A"                 ("Consolas" "DejaVu Sans Mono" "Symbola"  ))
    ("Latin Small Letter C with Curl"                 "Latin Small Letter C with Curl"              ("Consolas" "DejaVu Sans Mono" "Symbola"  ))
    ("Latin Small Letter Closed Reversed Open E"      "Latin Small Letter Closed Reversed Open E"   ("Consolas" "DejaVu Sans Mono" "Symbola"  ))
    ("Latin Small Letter Gamma"                       "Latin Small Letter Gamma"                    ("Consolas" "DejaVu Sans Mono" "Symbola"  ))
    ("Latin Small Letter Rams Horn"                   "Latin Small Letter Rams Horn"                ("Consolas" "DejaVu Sans Mono" "Symbola"  ))
    ("Latin Small Letter L with Belt"                 "Latin Small Letter L with Belt"              ("Consolas" "DejaVu Sans Mono" "Symbola"  ))
    ("Latin Small Letter Closed Omega"                "Latin Small Letter Closed Omega"             ("Consolas" "DejaVu Sans Mono" "Symbola"  ))
    ("Latin Small Letter Esh with Curl"               "Latin Small Letter Esh with Curl"            ("Consolas" "DejaVu Sans Mono" "Symbola"  ))
    ("Latin Small Letter V with Hook"                 "Latin Small Letter V with Hook"              ("Consolas" "DejaVu Sans Mono" "Symbola"  ))
    ("Latin Small Letter Z with Retroflex Hook"       "Latin Small Letter Z with Retroflex Hook"    ("Consolas" "DejaVu Sans Mono" "Symbola"  ))
    ("Latin Small Letter Z with Curl"                 "Latin Small Letter Z with Curl"              ("Consolas" "DejaVu Sans Mono" "Symbola"  ))
    ("Latin Small Letter Ezh with Curl"               "Latin Small Letter Ezh with Curl"            ("Consolas" "DejaVu Sans Mono" "Symbola"  ))
    ("Latin Small Letter Closed Open E"               "Latin Small Letter Closed Open E"            ("Consolas" "DejaVu Sans Mono" "Symbola"  ))
    ("Latin Letter Small Capital G with Hook"         "Latin Letter Small Capital G with Hook"      ("Consolas" "DejaVu Sans Mono" "Symbola"  ))
    ("Latin Small Letter J with Crossed-Tail"         "Latin Small Letter J with Crossed-Tail"      ("Consolas" "DejaVu Sans Mono" "Symbola"  ))
    ("Latin Small Letter Dezh Digraph"                "Latin Small Letter Dezh Digraph"             ("Consolas" "DejaVu Sans Mono" "Symbola"  ))
    ("Latin Small Letter Dz Digraph with Curl"        "Latin Small Letter Dz Digraph with Curl"     ("Consolas" "DejaVu Sans Mono" "Symbola"  ))
    ("Latin Small Letter Tc Digraph with Curl"        "Latin Small Letter Tc Digraph with Curl"     ("Consolas" "DejaVu Sans Mono" "Symbola"  ))
    ("Latin Letter Voiced Laryngeal Spirant"          "Latin Letter Ain"                            ("Consolas" "Quivira"                     ))
    ("Modifier Letter Small A"                        "Modifier Letter Small Turned A"              ("Consolas" "DejaVu Sans Mono" "Quivira"  ))
    ("Modifier Letter Small Open E"                   "Modifier Letter Small Turned Open E"         ("Consolas" "DejaVu Sans Mono" "Quivira"  ))
    ("Modifier Letter Small Ain"                      "Modifier Letter Small Chi"                   ("Consolas" "Quivira"                     ))
    ("Greek Subscript Small Letter Beta"              "Greek Subscript Small Letter Chi"            ("Consolas" "Quivira"                     ))
    ("Latin Small Letter Insular G"                   "Latin Small Letter Insular G"                ("Consolas" "Quivira"                     ))

    ;; Superscripts and Subscripts block
    ("Latin Subscript Small Letter A"                 "Latin Subscript Small Letter Schwa"          ("Consolas" "DejaVu Sans Mono" "Symbola"  ))
    ("Latin Subscript Small Letter H"                 "Latin Subscript Small Letter T"              (           "DejaVu Sans Mono" "Symbola"  ))

    ;; Spacing Modifier Letters block
    ("Modifier Letter Small Gamma"                    "Modifier Letter Small Gamma"                 ("Consolas" "DejaVu Sans Mono" "Symbola"  ))

    ;; Latin Extended-B block
    ("Latin Capital Letter G with Hook"               "Latin Small Letter Hv"                       ("Consolas" "DejaVu Sans Mono" "DejaVu Sans:width=condensed"  ))
    ("Latin Capital Letter Oi"                        "Latin Capital Letter P with Hook"            ("Consolas" "DejaVu Sans Mono" "DejaVu Sans:width=condensed"  ))
    ("Latin Capital Letter V with Hook"               "Latin Small Letter Y with Hook"              ("Consolas" "DejaVu Sans Mono" "DejaVu Sans:width=condensed"  ))
    ("Latin Capital Letter Tone Five"                 "Latin Letter Wynn"                           ("Consolas" "DejaVu Sans Mono" "DejaVu Sans:width=condensed"  ))
    ("Latin Capital Letter Yogh"                      "Latin Small Letter Yogh"                     ("Consolas" "DejaVu Sans Mono" "DejaVu Sans:width=condensed"  ))
    ("Latin Small Letter L with Curl"                 "Latin Small Letter T with Curl"              ("Consolas" "DejaVu Sans Mono" "DejaVu Sans:width=condensed"  ))
    ("Latin Capital Letter B with Hook"               "Latin Capital Letter B with Hook"            ("Consolas" "DejaVu Sans Mono" "DejaVu Sans:width=condensed"  ))
    ("Latin Capital Letter C with Hook"               "Latin Capital Letter C with Hook"            ("Consolas" "DejaVu Sans Mono" "DejaVu Sans:width=condensed"  ))
    ("Latin Small Letter C with Hook"                 "Latin Small Letter C with Hook"              ("Consolas" "DejaVu Sans Mono" "DejaVu Sans:width=condensed"  ))
    ("Latin Capital Letter D with Hook"               "Latin Capital Letter D with Hook"            ("Consolas" "DejaVu Sans Mono" "DejaVu Sans:width=condensed"  ))
    ("Latin Small Letter Turned Delta"                "Latin Small Letter Turned Delta"             ("Consolas" "DejaVu Sans Mono" "DejaVu Sans:width=condensed"  ))
    ("Latin Capital Letter K with Hook"               "Latin Capital Letter K with Hook"            ("Consolas" "DejaVu Sans Mono" "DejaVu Sans:width=condensed"  ))
    ("Latin Small Letter Lambda with Stroke"          "Latin Small Letter Lambda with Stroke"       ("Consolas" "DejaVu Sans Mono" "DejaVu Sans:width=condensed"  ))
    ("Latin Letter Yr"                                "Latin Letter Yr"                             ("Consolas" "DejaVu Sans Mono" "DejaVu Sans:width=condensed"  ))
    ("Latin Letter Reversed Esh Loop"                 "Latin Letter Reversed Esh Loop"              ("Consolas" "DejaVu Sans Mono" "DejaVu Sans:width=condensed"  ))
    ("Latin Capital Letter T with Hook"               "Latin Capital Letter T with Hook"            ("Consolas" "DejaVu Sans Mono" "DejaVu Sans:width=condensed"  ))
    ("Latin Small Letter Ezh with Tail"               "Latin Small Letter Ezh with Tail"            ("Consolas" "DejaVu Sans Mono" "DejaVu Sans:width=condensed"  ))
    ("Latin Capital Letter Hwair"                     "Latin Capital Letter Hwair"                  ("Consolas" "DejaVu Sans Mono" "DejaVu Sans:width=condensed"  ))
    ("Latin Small Letter S with Swash Tail"           "Latin Small Letter S with Swash Tail"        ("Consolas" "DejaVu Sans Mono" "DejaVu Sans:width=condensed"  ))
    ("Latin Small Letter Z with Swash Tail"           "Latin Small Letter Z with Swash Tail"        ("Consolas" "DejaVu Sans Mono" "DejaVu Sans:width=condensed"  ))
    ("Latin Capital Letter Wynn"                      "Latin Capital Letter Wynn"                   ("Consolas" "DejaVu Sans:width=condensed"                     ))
    ("Latin Small Letter Ou"                          "Latin Small Letter Ou"                       ("Consolas" "DejaVu Sans:width=condensed"                     ))
    ("Latin Small Letter Glottal Stop"                "Latin Small Letter Y with Stroke"            ("Consolas" "DejaVu Sans:width=condensed"                     ))

    ;; Latin Extended Additional block
    ("Latin Small Letter Long S with Diagonal Stroke" "Latin Small Letter Delta"                    ("DejaVu Sans:width=condensed" "Quivira"))
    ("Latin Capital Letter Middle-Welsh Ll"           "Latin Small Letter Y with Loop"              ("Quivira"                              ))

    ;; Enclosed CJK Letters and Months block
    ("Circled Hangul Kiyeok"                          "Korean Standard Symbol"                      ("PCMyungjo" "PilGi" "Malgun Gothic"))          ; Korean symbols

    ;; Halfwidth and Fullwidth Forms block
    ("Halfwidth Ideographic Full Stop"                "Halfwidth Katakana Semi-Voiced Sound Mark"   ("Osaka:spacing=m" "Meiryo" "HanaMinA"))        ; Japanese letters

    ;; Greek and Coptic block
    ("Coptic Capital Letter Shei"                     "Coptic Small Letter Dei"                     ("Microsoft Sans Serif" "DejaVu Sans:width=condensed"))

    ;; Alphabetic Presentation Forms block
    ("Hebrew Letter Yod with Hiriq"                   "Hebrew Ligature Alef Lamed"                  ("Miriam Fixed" "Ezra SIL" "Ezra SIL SR" "Arial Hebrew" "Adobe Hebrew" "Arial Unicode MS" "Quivira"))
    ("Armenian Small Ligature Men Now"                "Armenian Small Ligature Men Xeh"             ("Noto Sans Armenian" "Mshtakan" "Sylfaen" "DejaVu Sans:width=condensed" "Quivira" "Arial Unicode MS" ))

    ;; Arabic letters
    ("Arabic Letter Hah with Small Arabic Letter Tah Below" "Arabic Letter Kaf with Two Dots Above" ("Geeza Pro"                         ))

    ;; Cyrillic Supplement block
    ("Cyrillic Capital Letter Lha"                    "Cyrillic Small Letter Pe with Descender"      ("DejaVu Sans:width=condensed" "Doulos SIL" "Symbola" "Quivira"))
    ("Cyrillic Capital Letter Shha with Descender"    "Cyrillic Small Letter Shha with Descender"    ("Doulos SIL" "Symbola" "Quivira"    ))
    (#x528                                            #x52F                                          ("Symbola" "Quivira"                 ))

    ;; Cyrillic block
    ("Cyrillic Capital Letter Omega"                     "Cyrillic Small Letter Omega"                          ("Consolas" "DejaVu Sans:width=condensed" "Symbola"))
    ("Cyrillic Capital Letter Iotified E"                "Cyrillic Small Letter Psi"                            ("Consolas" "DejaVu Sans:width=condensed" "Symbola"))
    ("Cyrillic Capital Letter Izhitsa"                   "Cyrillic Small Letter Er With Tick"                   ("Consolas" "DejaVu Sans:width=condensed" "Symbola"))
    ("Cyrillic Capital Letter Ghe with Middle Hook"      "Cyrillic Small Letter Ghe with Middle Hook"           ("Consolas" "DejaVu Sans:width=condensed" "Symbola"))
    ("Cyrillic Capital Letter Ka With Vertical Stroke"   "Cyrillic Small Letter Bashkir Ka"                     ("Consolas" "DejaVu Sans:width=condensed" "Symbola"))
    ("Cyrillic Capital Letter Pe With Middle Hook"       "Cyrillic Small Letter Abkhasian Ha"                   ("Consolas" "DejaVu Sans:width=condensed" "Symbola"))
    ("Cyrillic Capital Ligature Te Tse"                  "Cyrillic Small Letter Che With Vertical Stroke"       ("Consolas" "DejaVu Sans:width=condensed" "Symbola"))
    ("Cyrillic Capital Letter Abkhasian Che"             "Cyrillic Small Letter Abkhasian Che With Descender"   ("Consolas" "DejaVu Sans:width=condensed" "Symbola"))
    ("Cyrillic Capital Letter Ka with Hook"              "Cyrillic Small Letter Palochka"                       ("Consolas" "DejaVu Sans:width=condensed" "Symbola"))
    ("Cyrillic Capital Letter Abkhasian Dze"             "Cyrillic Small Letter Abkhasian Dze"                  ("Consolas" "DejaVu Sans:width=condensed" "Symbola"))
    ("Cyrillic Capital Letter Ghe with Descender"        "Cyrillic Small Letter Ghe with Descender"             ("Consolas" "DejaVu Sans:width=condensed" "Symbola"))
    ("Cyrillic Capital Letter Ghe With Stroke And Hook"  "Cyrillic Small Letter Ha With Stroke"                 ("Consolas" "DejaVu Sans:width=condensed" "Symbola"))

    ;; Number forms block (making the vulgar fractions monospace if possible)
    ("Vulgar Fraction One Third"    "Vulgar Fraction Seven Eighths"  ("Consolas" "DejaVu Sans Mono"      ))

    ;; Letterlike Symbols block
    ("Account Of"                   "Addressed To The Subject"       ("Apple Symbols" "Symbola" "Quivira"))
    ("Cada Una"                     "Cada Una"                       ("Apple Symbols" "Symbola" "Quivira"))
    ("Prescription Take"            "Telephone Sign"                 ("Apple Symbols" "Symbola" "Quivira"))
    ("Versicle"                     "Versicle"                       ("Apple Symbols" "Symbola" "Quivira"))
    ("Turned Capital F"             "Turned Capital F"               ("Apple Symbols" "Symbola" "Quivira"))
    ("Facsimile Sign"               "Facsimile Sign"                 ("Apple Symbols" "Symbola" "Quivira"))
    ("Double-Struck Small Pi"       "Double-Struck Small Pi"         ("Symbola" "Quivira"                ))
    ("Per Sign"                     "Per Sign"                       ("Symbola" "Quivira"                ))
    ("Symbol For Samaritan Source"  "Symbol For Samaritan Source"    ("Symbola" "Quivira"                ))

    ;; Greek and Coptic block
    ("Greek Capital Letter Heta"                "Greek Small Letter Archaic Sampi"       ("DejaVu Sans:width=condensed" "Symbola" "Quivira"))
    ("Greek Capital Letter Pamphylian Digamma"  "Greek Small Letter Pamphylian Digamma"  ("DejaVu Sans:width=condensed" "Symbola" "Quivira"))
    ("Greek Capital Kai Symbol"                 "Greek Capital Kai Symbol"               ("DejaVu Sans:width=condensed" "Symbola" "Quivira"))

    ;; Old Italic block
    ("Old Italic Letter Ess"                    "Old Italic Letter Ess"                  ("Albanian")))

  "Overrides for `unicode-fonts-block-font-mapping' over arbitrary ranges.

Ranges are specified using the full UCS name or UCS number of
the start and end characters.  To override just one character,
give the same value for both endpoints.

These mappings are only installed in Emacs if a preferred font
for the range is available on your system.  When multiple fonts
are given, each is tried in order.

If the font backend provided by your operating system handles
glyph-by-glyph fallthrough well, you may not need many of
these mappings."
  :type '(alist :key-type (choice (string :tag "Start Character Name")
                                  (integer :tag "Start Character Number"))
                :value-type (group (choice (string :tag "End Character Name")
                                           (integer :tag "End Character Number"))
                                   (repeat :tag "Preferred Fonts" (string :tag ""))))
  :group 'unicode-fonts)

;;; variables

(defvar unicode-fonts-setup-done              nil "Fontsets for which unicode-font setup is complete.")
(defvar unicode-fonts-skipped-fonts-computed  nil "The computed extension of `unicode-fonts-skip-fonts'.")
(defvar unicode-fonts--instructions           nil "Alist of code to set up fonts on a given system.")

;; note: variable outside unicode-fonts- namespace
(defvar unicode-block-history                 nil "History of Unicode blocks entered in the minibuffer.")

;;; compatibility functions

(defun persistent-softest-store (symbol value location &optional expiration)
  "Call `persistent-soft-store' but don't fail when library not present."
  (ignore-errors (persistent-soft-store symbol value location expiration)))
(defun persistent-softest-fetch (symbol location)
  "Call `persistent-soft-fetch' but don't fail when library not present."
  (ignore-errors (persistent-soft-fetch symbol location)))
(defun persistent-softest-exists-p (symbol location)
  "Call `persistent-soft-exists-p' but don't fail when library not present."
  (ignore-errors (persistent-soft-exists-p symbol location)))
(defun persistent-softest-flush (location)
  "Call `persistent-soft-flush' but don't fail when library not present."
  (ignore-errors (persistent-soft-flush location)))
(defun persistent-softest-location-readable (location)
  "Call `persistent-soft-location-readable' but don't fail when library not present."
  (ignore-errors (persistent-soft-location-readable location)))
(defun persistent-softest-location-destroy (location)
  "Call `persistent-soft-location-destroy' but don't fail when library not present."
  (ignore-errors (persistent-soft-location-destroy location)))

;;; utility functions

;;;###autoload
(defun unicode-fonts-first-existing-font (font-names)
  "Return the (normalized) first existing font name from FONT-NAMES.

FONT-NAMES is a list, with each element typically in Fontconfig
font-name format.

The font existence-check is lazy; fonts after the first hit are
not checked."
  (font-utils-first-existing-font (remove-if #'(lambda (x)
                                                    (member* x unicode-fonts-skipped-fonts-computed :test 'font-utils-lenient-name-equal))
                                                font-names)))

;;;###autoload
(defun unicode-fonts-font-exists-p (font-name &optional point-size strict)
  "Run `font-utils-exists-p' with a limited scope.

The scope is defined by `unicode-fonts-restrict-to-fonts'.

FONT-NAME, POINT-SIZE, and STRICT are as documented at
`font-utils-exists-p'."
  (font-utils-exists-p font-name point-size strict unicode-fonts-restrict-to-fonts))

(defsubst unicode-fonts--create-char-range (range)
  "Create a numeric character range from RANGE.

RANGE is a list of two UCS character representations, either in
the form of integer code points or Unicode character names.

The return value is a list of two ascending integers, or nil on
error."
  (let ((return-range (copy-tree range)))
    (when (stringp (car return-range))
      (setf (car return-range) (ucs-utils-char (car return-range) nil)))
    (when (stringp (cadr return-range))
      (setf (cadr return-range) (ucs-utils-char (cadr return-range) nil)))
    (if (and (integerp (car return-range)) (integerp (cadr return-range)))
        (progn
          (when (> (car return-range) (cadr return-range))
            (setq return-range (nreverse return-range)))
          return-range)
      nil)))

(defun unicode-fonts-compute-skipped-fonts ()
  "Compute list of fonts to skip from consideration."
  (setq unicode-fonts-skipped-fonts-computed unicode-fonts-skip-fonts)
  (dolist (cell unicode-fonts-known-font-characteristics)
    (let ((name (car cell))
          (props (cdr cell)))
      (setq name (replace-regexp-in-string ":.*\\'" "" name))
      (when (and (memq 'chinese-traditional unicode-fonts-skip-font-groups)
                 (eq 'traditional (plist-get props :chinese)))
        (push name unicode-fonts-skipped-fonts-computed))
      (when (and (memq 'chinese-simplified unicode-fonts-skip-font-groups)
                 (eq 'simplified (plist-get props :chinese)))
        (push name unicode-fonts-skipped-fonts-computed))
      (when (and (memq 'chinese-hanja unicode-fonts-skip-font-groups)
                 (eq 'hanja (plist-get props :chinese)))
        (push name unicode-fonts-skipped-fonts-computed))
      (when (and (memq 'chinese-kanji unicode-fonts-skip-font-groups)
                 (eq 'kanji (plist-get props :chinese)))
        (push name unicode-fonts-skipped-fonts-computed))
      (when (and (memq 'chinese-nom unicode-fonts-skip-font-groups)
                 (eq 'nom (plist-get props :chinese)))
        (push name unicode-fonts-skipped-fonts-computed))
      (when (and (memq 'arabic-naskh unicode-fonts-skip-font-groups)
                 (eq 'naskh (plist-get props :arabic)))
        (push name unicode-fonts-skipped-fonts-computed))
      (when (and (memq 'arabic-diwani unicode-fonts-skip-font-groups)
                 (eq 'diwani (plist-get props :arabic)))
        (push name unicode-fonts-skipped-fonts-computed))
      (when (and (memq 'arabic-farsi unicode-fonts-skip-font-groups)
                 (eq 'farsi (plist-get props :arabic)))
        (push name unicode-fonts-skipped-fonts-computed))
      (when (and (memq 'arabic-urdu unicode-fonts-skip-font-groups)
                 (eq 'urdu (plist-get props :arabic)))
        (push name unicode-fonts-skipped-fonts-computed))
      (when (and (memq 'arabic-kufic unicode-fonts-skip-font-groups)
                 (eq 'kufic (plist-get props :arabic)))
        (push name unicode-fonts-skipped-fonts-computed))
      (when (and (memq 'microsoft-only unicode-fonts-skip-font-groups)
                 (equal '(microsoft) (plist-get props :licenses)))
        (push name unicode-fonts-skipped-fonts-computed))
      (when (and (memq 'apple-only unicode-fonts-skip-font-groups)
                 (equal '(apple) (plist-get props :licenses)))
        (push name unicode-fonts-skipped-fonts-computed))
      (when (and (memq 'multicolor unicode-fonts-skip-font-groups)
                 (eq 'multi (plist-get props :color)))
        (push name unicode-fonts-skipped-fonts-computed))
      (when (and (memq 'microsoft unicode-fonts-skip-font-groups)
                 (memq 'microsoft (plist-get props :licenses)))
        (push name unicode-fonts-skipped-fonts-computed))
      (when (and (memq 'apple unicode-fonts-skip-font-groups)
                 (memq 'apple (plist-get props :licenses)))
        (push name unicode-fonts-skipped-fonts-computed))
      (when (and (memq 'free unicode-fonts-skip-font-groups)
                 (memq 'free (plist-get props :licenses)))
        (push name unicode-fonts-skipped-fonts-computed))
      (when (and (memq 'non-free unicode-fonts-skip-font-groups)
                 (not (memq 'free (plist-get props :licenses))))
        (push name unicode-fonts-skipped-fonts-computed))
      (when (and (memq 'low-quality-glyphs unicode-fonts-skip-font-groups)
                 (eq 'low (plist-get props :glyph-quality)))
        (push name unicode-fonts-skipped-fonts-computed))
      (when (and (memq 'decorative unicode-fonts-skip-font-groups)
                 (eq t (plist-get props :decorative)))
        (push name unicode-fonts-skipped-fonts-computed))
      (when (and (memq 'non-cleartype unicode-fonts-skip-font-groups)
                 (not (eq t (plist-get props :cleartype))))
        (push name unicode-fonts-skipped-fonts-computed))
      (when (and (memq 'buggy-before-vista unicode-fonts-skip-font-groups)
                 (eq t (plist-get props :buggy-before-vista)))
        (push name unicode-fonts-skipped-fonts-computed))
      (delete-dups unicode-fonts-skipped-fonts-computed))))

;;;###autoload
(defun unicode-fonts-read-block-name (&optional ido)
  "Read a Unicode block name using `completing-read'.

Spaces are replaced with underscores in completion values, but
are removed from the return value.

Use `ido-completing-read' if IDO is set."
  (save-match-data
    (let ((prompt "Block: ")
          (reader (if ido 'ido-completing-read 'completing-read))
          (block-names (mapcar #'(lambda (x)
                                   (replace-regexp-in-string " " "_" x))
                               (mapcar 'car unicode-fonts-blocks))))
      (replace-regexp-in-string "_" " "
         (funcall reader prompt block-names nil nil nil 'unicode-block-history)))))

;;; debugging functions

(defun unicode-fonts-debug-info-at-point ()
  "Display debug info about the character at point."
  (when (char-after)
    (let ((font (font-at (point)))
          (font-name nil)
          (font-size nil)
          (block-name nil)
          (plane-name nil)
          (char-name (ucs-utils-pretty-name (char-after))))
      (when font
        (setq font-name (or (font-get font :name)
                            (font-get font :family)))
        (when (and font-name
                   (symbolp font-name))
          (setq font-name (symbol-name font-name)))
        (setq font-size (font-get font :size))
        (when (numberp font-size)
          (setq font-size (number-to-string font-size)))
        (when (and (stringp font-name)
                   (stringp font-size)
                   (> (length font-name) 0)
                   (> (length font-size) 0))
          (callf concat font-name "-" font-size))
        (unless (and (stringp font-name)
                     (> (length font-name) 0))
          (setq font-name (font-utils-name-from-xlfd (font-xlfd-name font)))))
      (setq block-name
           (catch 'bn
             (dolist (cell unicode-fonts-blocks)
               (let* ((block-name (car cell))
                      (char-range (cdr cell)))
                 (when (and (>= (char-after) (car char-range))
                            (<= (char-after) (cadr char-range)))
                   (throw 'bn block-name))))))
      (setq plane-name
           (catch 'pn
             (dolist (cell unicode-fonts-planes)
               (let* ((plane-name (car cell))
                      (char-range (cdr cell)))
                 (when (and (>= (char-after) (car char-range))
                            (<= (char-after) (cadr char-range)))
                   (throw 'pn plane-name))))))
      (message "font: %s / block: %s / plane: %s / char: %s" font-name block-name plane-name char-name))))

(defun unicode-fonts-debug-change-font-for-block (&optional block-name font-name)
  "Calling this command can crash Emacs.

Temporarily change the font used for BLOCK-NAME to FONT-NAME.

To permanently change the font for BLOCK-NAME, use the
customization interface."
  (callf or block-name (unicode-fonts-read-block-name 'ido))
  (callf or font-name (font-utils-read-name 'ido))
  (assert (assoc-string block-name unicode-fonts-blocks 'case-fold) nil "No such block")
  (assert (unicode-fonts-font-exists-p font-name) nil "Font does not is exist or is not understood: %s" font-name)
  (when (y-or-n-p (propertize "Really risk crashing Emacs?" 'face 'highlight))
    (message "")
    (let ((char-range (cdr (assoc-string block-name unicode-fonts-blocks 'case-fold))))
      (dolist (fontset-name (remove-if-not #'(lambda (fs) (ignore-errors (fontset-info fs))) unicode-fonts-fontset-names))
        (set-fontset-font fontset-name
                          (cons (decode-char 'ucs (car char-range)) (decode-char 'ucs (cadr char-range)))
                          (font-spec :name (concat font-name ":") :registry "iso10646-1"))))))

(defun unicode-fonts-debug-change-all-fonts (&optional font-name)
  "Calling this command can crash Emacs.

Temporarily change the font used for all blocks to FONT-NAME."
  (callf or font-name (font-utils-read-name 'ido))
  (assert (unicode-fonts-font-exists-p font-name) nil "Font does not is exist or is not understood: %s" font-name)
  (when (y-or-n-p (propertize "Really risk crashing Emacs?" 'face 'highlight))
    (dolist (fontset-name (remove-if-not #'(lambda (fs) (ignore-errors (fontset-info fs))) unicode-fonts-fontset-names))
      (dolist (cell unicode-fonts-block-font-mapping)
        (let* ((block-name (car cell))
               (char-range (cdr (assoc-string block-name unicode-fonts-blocks 'case-fold))))
          (when char-range
            (set-fontset-font fontset-name
                              (cons (decode-char 'ucs (car char-range)) (decode-char 'ucs (cadr char-range)))
                              (font-spec :name (concat font-name ":") :registry "iso10646-1"))))))))

(defun unicode-fonts-debug-interactively (&optional arg)
  "Always show the font at point.

This is a buffer-local setting.  Turn it off by quitting the
buffer or calling this function with negative ARG."
  (if (and (numberp arg)
           (< arg 0))
      (remove-hook 'post-command-hook 'unicode-fonts-debug-info-at-point t)
    ;; else
    (add-hook 'post-command-hook 'unicode-fonts-debug-info-at-point t t)))

(defun unicode-fonts-debug-insert-block (&optional block-name)
  "Insert all the characters from BLOCK-NAME for debugging purposes.

See also: `list-charset-chars'."
  (callf or block-name (unicode-fonts-read-block-name 'ido))
  (set-buffer-multibyte t)
  (cond
    ((eq block-name 'all)
     (dolist (name (reverse (mapcar 'car unicode-fonts-blocks)))
       (unicode-fonts-debug-insert-block name)))
    ((eq block-name 'all-cjk)
     (dolist (name (reverse '(
                     "Bopomofo Extended"
                     "Bopomofo"
                     "CJK Compatibility"
                     "CJK Compatibility Forms"
                     "CJK Compatibility Ideographs Supplement"
                     "CJK Compatibility Ideographs"
                     "CJK Radicals Supplement"
                     "CJK Strokes"
                     "CJK Symbols and Punctuation"
                     "CJK Unified Ideographs"
                     "CJK Unified Ideographs Extension A"
                     "CJK Unified Ideographs Extension B"
                     "CJK Unified Ideographs Extension C"
                     "CJK Unified Ideographs Extension D"
                     "CJK Unified Ideographs Extension E"
                     "Enclosed CJK Letters and Months"
                     "Enclosed Ideographic Supplement"
                     "Ideographic Description Characters"
                     "Hangul Compatibility Jamo"
                     "Hangul Jamo"
                     "Hangul Jamo Extended-A"
                     "Hangul Jamo Extended-B"
                     "Hangul Syllables"
                     "Kana Supplement"
                     "Kanbun"
                     "Kangxi Radicals"
                     "Katakana"
                     "Katakana Phonetic Extensions"
                     "Hiragana"
                     "Modifier Tone Letters"
                     "Yi Radicals"
                     "Yi Syllables")))
       (unicode-fonts-debug-insert-block name)))
    ((eq block-name 'all-greek)
     (dolist (name (reverse '(
                     "Ancient Greek Musical Notation"
                     "Ancient Greek Numbers"
                     "Greek and Coptic"
                     "Greek Extended"
                     "Linear A"
                     "Linear B Ideograms"
                     "Linear B Syllabary")))
       (unicode-fonts-debug-insert-block name)))
    ((eq block-name 'all-math)
     (dolist (name (reverse '(
                     "Letterlike Symbols"
                     "Mathematical Alphanumeric Symbols"
                     "Mathematical Operators"
                     "Miscellaneous Mathematical Symbols-A"
                     "Miscellaneous Mathematical Symbols-B"
                     "Miscellaneous Technical"
                     "Number Forms"
                     "Superscripts and Subscripts"
                     "Supplemental Arrows-A"
                     "Supplemental Arrows-B"
                     "Supplemental Arrows-C"
                     "Supplemental Mathematical Operators")))
       (unicode-fonts-debug-insert-block name)))
    ((eq block-name 'all-arabic)
     (dolist (name (reverse '(
                     "Arabic"
                     "Arabic Extended-A"
                     "Arabic Mathematical Alphabetic Symbols"
                     "Arabic Presentation Forms-A"
                     "Arabic Presentation Forms-B"
                     "Arabic Supplement")))
       (unicode-fonts-debug-insert-block name)))
    ((eq block-name 'all-cyrillic)
     (dolist (name (reverse '(
                     "Cyrillic"
                     "Cyrillic Extended-A"
                     "Cyrillic Extended-B"
                     "Cyrillic Supplement")))
       (unicode-fonts-debug-insert-block name)))
    ((eq block-name 'all-ethiopic)
     (dolist (name (reverse '(
                     "Ethiopic"
                     "Ethiopic Extended"
                     "Ethiopic Extended-A"
                     "Ethiopic Supplement")))
       (unicode-fonts-debug-insert-block name)))
    ((eq block-name 'all-arrows)
     (dolist (name (reverse '(
                     "Arrows"
                     "Miscellaneous Symbols and Arrows"
                     "Supplemental Arrows-A"
                     "Supplemental Arrows-B"
                     "Supplemental Arrows-C"
                     "Transport and Map Symbols")))
       (unicode-fonts-debug-insert-block name)))
    ((eq block-name 'all-symbols)
     (dolist (name (reverse '(
                     "Aegean Numbers"
                     "Alchemical Symbols"
                     "Alphabetic Presentation Forms"
                     "Ancient Greek Musical Notation"
                     "Ancient Greek Numbers"
                     "Ancient Symbols"
                     "Arabic Mathematical Alphabetic Symbols"
                     "Arrows"
                     "Block Elements"
                     "Box Drawing"
                     "Byzantine Musical Symbols"
                     "CJK Symbols and Punctuation"
                     "Combining Diacritical Marks for Symbols"
                     "Common Indic Number Forms"
                     "Control Pictures"
                     "Coptic Epact Numbers"
                     "Counting Rod Numerals"
                     "Cuneiform Numbers and Punctuation"
                     "Currency Symbols"
                     "Dingbats"
                     "Domino Tiles"
                     "Emoticons"
                     "Enclosed Alphanumeric Supplement"
                     "Enclosed Alphanumerics"
                     "Enclosed CJK Letters and Months"
                     "Enclosed Ideographic Supplement"
                     "General Punctuation"
                     "Geometric Shapes"
                     "Geometric Shapes Extended"
                     "Halfwidth and Fullwidth Forms"
                     "IPA Extensions"
                     "Khmer Symbols"
                     "Latin-1 Supplement"
                     "Letterlike Symbols"
                     "Mahjong Tiles"
                     "Mathematical Alphanumeric Symbols"
                     "Mathematical Operators"
                     "Miscellaneous Mathematical Symbols-A"
                     "Miscellaneous Mathematical Symbols-B"
                     "Miscellaneous Symbols"
                     "Miscellaneous Symbols and Arrows"
                     "Miscellaneous Symbols and Pictographs"
                     "Miscellaneous Technical"
                     "Musical Symbols"
                     "Number Forms"
                     "Optical Character Recognition"
                     "Ornamental Dingbats"
                     "Playing Cards"
                     "Rumi Numeral Symbols"
                     "Sinhala Archaic Numbers"
                     "Small Form Variants"
                     "Spacing Modifier Letters"
                     "Specials"
                     "Superscripts and Subscripts"
                     "Supplemental Arrows-A"
                     "Supplemental Arrows-B"
                     "Supplemental Arrows-C"
                     "Supplemental Mathematical Operators"
                     "Supplemental Punctuation"
                     "Supplemental Symbols and Pictographs"
                     "Tai Xuan Jing Symbols"
                     "Transport and Map Symbols"
                     "Yijing Hexagram Symbols")))
       (unicode-fonts-debug-insert-block name)))
    (t
     (save-match-data
       (let ((char-range (cdr (assoc-string block-name unicode-fonts-blocks 'case-fold)))
             (counter 0)
             (posn nil))
         (assert (assoc-string block-name unicode-fonts-blocks 'case-fold) nil "No such block")
         (unless (looking-at-p "^")
           (insert "\n"))
         (setq posn (point))
         (insert (replace-regexp-in-string " " "_" block-name) "\n-----\n")
         (loop for i from (car char-range) to (cadr char-range)
               do (progn
                    (insert (ucs-utils-char i)
                            "  "
                            (concat "#x" (upcase (format "%02x" i)))
                            "  "
                            "\"" (ucs-utils-pretty-name i) "\""
                            "\n")
                    (incf counter)
                    (when (eq 0 (% counter 16))
                      (insert "\n"))))
         (push-mark (point) t t)
         (goto-char posn))))))

(defun unicode-fonts-debug-check-duplicate-fonts (font-name font-list)
  "Test whether FONT-NAME occurs more than once in FONT-LIST.

Returns a list of duplicates when there is more than one
occurrence, otherwise nil."
  (let ((matches (copy-list (member* font-name font-list :test 'font-utils-lenient-name-equal)))
        (hits nil)
        (dupes nil))
    (setq matches (sort matches #'(lambda (a b)
                                    (equal a font-name))))
    (push (pop matches) dupes)
    (while (setq hits (copy-list (member* font-name matches :test 'font-utils-lenient-name-equal)))
      (let ((hit (pop hits)))
        (unless (font-utils-is-qualified-variant font-name hit)
          (push hit dupes)))
      (setq matches hits))
    (when (> (length dupes) 1)
      dupes)))

(defun unicode-fonts-debug-validate-data (&optional insert)
  "Validate `unicode-fonts-block-font-mapping' and other data.

With optional INSERT, insert debug information into the current
buffer instead of sending it to the *Messages* log."
  (let ((message-function 'message)
        (known-fonts (mapcar 'car unicode-fonts-known-font-characteristics))
        (reporter nil)
        (dupes nil)
        (counter 0)
        (all-override-ranges nil))
    (when insert
      (require 'express)
      (setq message-function 'express-message-insert))

    ;; known fonts
    (setq reporter (make-progress-reporter "Checking fonts for duplicates ... " 0 (length known-fonts)))
    (setq counter 0)
    (dolist (font known-fonts)
      (progress-reporter-update reporter (incf counter))
      (when (setq dupes (unicode-fonts-debug-check-duplicate-fonts font known-fonts))
        (funcall message-function "\n-----\nFont %s\n-----" font)
        (funcall message-function "ERROR: font occurs at least twice in known fonts: %s" dupes)))
    (progress-reporter-done reporter)

    ;; mappings
    (setq reporter (make-progress-reporter "Checking Unicode block mappings ... " 0 (length unicode-fonts-block-font-mapping)))
    (setq counter 0)
    (dolist (cell unicode-fonts-block-font-mapping)
      (progress-reporter-update reporter (incf counter))
      (let* ((block-name (car cell))
             (char-range (cdr (assoc-string block-name unicode-fonts-blocks 'case-fold)))
             (all-fonts-with-qualifiers (cadr cell))
             (all-fonts (mapcar #'(lambda (x) (replace-regexp-in-string ":.*\\'" "" x)) all-fonts-with-qualifiers))
             (existing-fonts (remove-if-not 'unicode-fonts-font-exists-p all-fonts))
             (existing-unskipped-fonts (remove-if #'(lambda (x)
                                                      (member* x unicode-fonts-skipped-fonts-computed :test 'font-utils-lenient-name-equal)) existing-fonts))
             (best-font (pop existing-unskipped-fonts))
             (licenses nil))
        (funcall message-function "\n-----\nBlock %s\n-----" block-name)
        (dolist (qualified-font all-fonts-with-qualifiers)
          (let ((font (replace-regexp-in-string ":.*\\'" "" qualified-font)))
            (when (setq dupes (unicode-fonts-debug-check-duplicate-fonts qualified-font all-fonts-with-qualifiers))
              (funcall message-function "ERROR: font occurs at least twice in block: %s" dupes))
            (let ((plist (cdr (assoc-string font unicode-fonts-known-font-characteristics))))
              (if plist
                  (setq licenses (append licenses (plist-get plist :licenses)))
                (funcall message-function "ERROR: Font %s is not listed" font)))))
        (unless (memq 'microsoft licenses)
          (funcall message-function "No Microsoft font for block %s" block-name))
        (unless (memq 'free licenses)
          (funcall message-function "No Free font for block %s" block-name))
        (unless (memq 'apple licenses)
          (funcall message-function "No Apple font for block %s" block-name))
        (unless existing-fonts
          (funcall message-function "No displayable font on this system for %s" block-name))
        (unless best-font
          (funcall message-function "No acceptable font on this system for %s" block-name))))
    (progress-reporter-done reporter)

    ;; overrides
    (setq reporter (make-progress-reporter "Checking overrides ... " 0 (length unicode-fonts-overrides-mapping)))
    (setq counter 0)
    (dolist (cell unicode-fonts-overrides-mapping)
      (progress-reporter-update reporter (incf counter))
      (let* ((char-range (unicode-fonts--create-char-range (list (car cell) (cadr cell))))
             (all-fonts (mapcar #'(lambda (x) (replace-regexp-in-string ":.*\\'" "" x)) (car (last cell))))
             (existing-fonts (remove-if-not 'unicode-fonts-font-exists-p all-fonts))
             (existing-unskipped-fonts (remove-if #'(lambda (x)
                                                      (member* x unicode-fonts-skipped-fonts-computed :test 'font-utils-lenient-name-equal)) existing-fonts))
             (best-font (pop existing-unskipped-fonts))
             (licenses nil))
        (funcall message-function "\n-----\nOverride %s\n-----" (list (car cell) (cadr cell)))
        (if (not char-range)
            (funcall message-function (format "ERROR: invalid character range %s/%s" (car cell) (cadr cell)))
          ;; else
          (when (not (eq (ucs-utils-char (car cell) nil) (car char-range)))
            (funcall message-function "Warning: character range out of order"))
          (dolist (old-range all-override-ranges)
            (when (and (>= (car char-range) (car old-range))
                       (<= (car char-range) (cadr old-range)))
              (funcall message-function "ERROR: first element overlaps with another range"))
            (when (and (>= (cadr char-range) (car old-range))
                       (<= (cadr char-range) (cadr old-range)))
              (funcall message-function "ERROR: last element overlaps with another range")))
          (push char-range all-override-ranges)
          (dolist (font all-fonts)
            (when (setq dupes (unicode-fonts-debug-check-duplicate-fonts font all-fonts))
              (funcall message-function "ERROR: font occurs at least twice in override: %s" dupes))
            (let ((plist (cdr (assoc-string font unicode-fonts-known-font-characteristics))))
              (if plist
                  (setq licenses (append licenses (plist-get plist :licenses)))
                (funcall message-function "ERROR: Font %s is not listed" font))))
          ;; todo here check licenses for coverage as above
          )))
    (progress-reporter-done reporter)))

;;; driver for setup

(defun unicode-fonts--generate-instructions (fontset-name)
  "Generate `eval'able instructions for modifying FONTSET-NAME.

This is the principal driver for the library, which converts
`unicode-fonts-block-font-mapping' and other settings into as
series of `set-fontset-font' instructions.

Instructions for FONTSET-NAME will be placed in alist
`unicode-fonts--instructions'."
  (when (and (display-multi-font-p)
             (or (not (assoc fontset-name unicode-fonts--instructions))
                 (not (cdr (assoc fontset-name unicode-fonts--instructions)))))

    (setq unicode-fonts--instructions
          (delq (assoc fontset-name unicode-fonts--instructions) unicode-fonts--instructions))

    (let ((reporter nil)
          (counter 0)
          (instructions nil))

      ;; debug font availability
      (when unicode-fonts-debug-availability
        (let ((all-fonts nil)
              (current-msg (current-message))
              (message-log-max most-positive-fixnum))
          (dolist (cell unicode-fonts-block-font-mapping)
            (callf append all-fonts (cadr cell)))
          (delete-dups all-fonts)
          (setq reporter (make-progress-reporter "Unicode Fonts - Debugging Fonts ... " 0 (length all-fonts)))
          (setq counter 0)
          (dolist (font all-fonts)
            (progress-reporter-update reporter (incf counter))
            (unless (unicode-fonts-font-exists-p font)
              (message "Unicode-fonts: font not found: %s" font)))
          (message current-msg))
        (progress-reporter-done reporter))

      ;; first, install fallback mapping
      (let* ((fonts (remove-if #'(lambda (x)
                                   (member* x unicode-fonts-skipped-fonts-computed
                                            :test 'font-utils-lenient-name-equal))
                               unicode-fonts-fallback-font-list))
             (best-font nil))
        (cond
          ((eq unicode-fonts-existence-checks 'none)
           t)
          ((eq unicode-fonts-existence-checks 'first)
           (setq fonts (list (unicode-fonts-first-existing-font fonts))))
          (t    ; 'all
           (setq fonts (remove-if-not 'unicode-fonts-font-exists-p fonts))))
        (setq best-font (pop fonts))
        (when best-font
          (push `(set-fontset-font ,fontset-name nil (font-spec :name ,(concat best-font ":") :registry "iso10646-1")) instructions))
        (dolist (lesser-font fonts)
          (push `(set-fontset-font ,fontset-name nil (font-spec :name ,(concat lesser-font ":") :registry "iso10646-1") nil 'append) instructions)))

      ;; next, install mappings by unicode block
      ;; this is slow the first time through, because of unicode-fonts-font-exists-p
      (unless unicode-fonts-less-feedback
        (setq reporter (make-progress-reporter (format "Unicode Fonts - Mapping Unicode Blocks in %s ... " fontset-name) 0 (length unicode-fonts-block-font-mapping)))
        (setq counter 0))
      (dolist (cell unicode-fonts-block-font-mapping)
        (unless unicode-fonts-less-feedback
          (progress-reporter-update reporter (incf counter)))
        (let* ((block-name (car cell))
               (char-range (cdr (assoc-string block-name unicode-fonts-blocks 'case-fold)))
               (fonts (remove-if #'(lambda (x)
                                     (member* x unicode-fonts-skipped-fonts-computed
                                              :test 'font-utils-lenient-name-equal))
                                 (cadr cell)))
               (best-font nil))
        (when char-range
          (cond
            ((eq unicode-fonts-existence-checks 'none)
             t)
            ((eq unicode-fonts-existence-checks 'first)
             (setq fonts (list (unicode-fonts-first-existing-font fonts))))
            (t    ; 'all
             (setq fonts (remove-if-not 'unicode-fonts-font-exists-p fonts))))
          (setq best-font (pop fonts))
          (when best-font
            (push `(set-fontset-font ,fontset-name
                                     (quote ,(cons (decode-char 'ucs (car char-range)) (decode-char 'ucs (cadr char-range))))
                                     (font-spec :name ,(concat best-font ":") :registry "iso10646-1")) instructions))
          (dolist (lesser-font fonts)
            (push `(set-fontset-font ,fontset-name
                                     (quote ,(cons (decode-char 'ucs (car char-range)) (decode-char 'ucs (cadr char-range))))
                                     (font-spec :name ,(concat lesser-font ":") :registry "iso10646-1") nil 'append) instructions)))))
      (unless unicode-fonts-less-feedback
        (progress-reporter-done reporter))

      ;; finally, install "overrides", which are mappings over an arbitrary range
      (unless unicode-fonts-ignore-overrides
        (unless unicode-fonts-less-feedback
          (setq reporter (make-progress-reporter (format "Unicode Fonts - Mapping Overrides in %s ... " fontset-name) 0 (length unicode-fonts-overrides-mapping)))
          (setq counter 0))
        (dolist (cell unicode-fonts-overrides-mapping)
          (unless unicode-fonts-less-feedback
            (progress-reporter-update reporter (incf counter)))
          (let* ((char-range (unicode-fonts--create-char-range (list (car cell) (cadr cell))))
                 (fonts (remove-if #'(lambda (x)
                                       (member* x unicode-fonts-skipped-fonts-computed
                                                :test 'font-utils-lenient-name-equal))
                                   (car (last cell))))
                 (best-font nil))
            (when char-range
              (cond
                ((eq unicode-fonts-existence-checks 'none)
                 t)
                ((eq unicode-fonts-existence-checks 'first)
                 (setq fonts (list (unicode-fonts-first-existing-font fonts))))
                (t    ; 'all
                 (setq fonts (remove-if-not 'unicode-fonts-font-exists-p fonts))))
              (if unicode-fonts-use-prepend
                  (dolist (font (reverse fonts))
                    (push `(set-fontset-font ,fontset-name
                                             (quote ,(cons (decode-char 'ucs (car char-range)) (decode-char 'ucs (cadr char-range))))
                                             (font-spec :name ,(concat font ":") :registry "iso10646-1") nil 'prepend) instructions))
                ;; else
                (setq best-font (pop fonts))
                (when best-font
                  (push `(set-fontset-font ,fontset-name
                                           (quote ,(cons (decode-char 'ucs (car char-range)) (decode-char 'ucs (cadr char-range))))
                                           (font-spec :name ,(concat best-font ":") :registry "iso10646-1")) instructions))
                  (dolist (font fonts)
                    (push `(set-fontset-font ,fontset-name
                                             (quote ,(cons (decode-char 'ucs (car char-range)) (decode-char 'ucs (cadr char-range))))
                                             (font-spec :name ,(concat font ":") :registry "iso10646-1") nil 'append) instructions))))))
            (unless unicode-fonts-less-feedback
              (progress-reporter-done reporter)))
      (push (cons fontset-name (nreverse instructions)) unicode-fonts--instructions))))

(defun unicode-fonts--configuration-checksum ()
  "Return a global configuration checksum relevant to unicode-fonts.

This can be used to invalidate cached instructions if the system
has changed."
  (md5 (format "%s" (list
                     'font-utils-list-names                     (sort (font-utils-list-names) 'string<)
                     'unicode-fonts-block-font-mapping          unicode-fonts-block-font-mapping
                     'unicode-fonts-blocks                      unicode-fonts-blocks
                     'unicode-fonts-existence-checks            unicode-fonts-existence-checks
                     'unicode-fonts-fallback-font-list          unicode-fonts-fallback-font-list
                     'unicode-fonts-fontset-names               unicode-fonts-fontset-names
                     'unicode-fonts-ignore-overrides            unicode-fonts-ignore-overrides
                     'unicode-fonts-known-font-characteristics  (sort unicode-fonts-known-font-characteristics #'(lambda (x y) (string< (car x) (car y))))
                     'unicode-fonts-overrides-mapping           unicode-fonts-overrides-mapping
                     'unicode-fonts-planes                      unicode-fonts-planes
                     'unicode-fonts-restrict-to-fonts           unicode-fonts-restrict-to-fonts
                     'unicode-fonts-skip-font-groups            (sort unicode-fonts-skip-font-groups 'string<)
                     'unicode-fonts-skip-fonts                  (sort unicode-fonts-skip-fonts 'string<)
                     'unicode-fonts-use-prepend                 unicode-fonts-use-prepend))
       nil nil 'utf-8))

(defun unicode-fonts--load-or-generate-instructions (fontset-name &optional regenerate)
  "Load or generate `eval'able instructions for modifying FONTSET-NAME.

When possible (and according to the setting of the customizable
variable `unicode-fonts-use-persistent-storage'), instructions
will be loaded from a disk cache.

Optional REGENERATE requests that the disk cache be invalidated
and regenerated.

Instructions for FONTSET-NAME will be placed in alist
`unicode-fonts--instructions'."
  (when (display-multi-font-p)
    (let* ((cache-id (format "f:%s-w:%s-h:%s-e:%s-l:%s"
                             fontset-name
                             window-system
                             (font-utils-client-hostname)
                             emacs-version
                             (get 'unicode-fonts 'custom-version)))
           (checksum-key     (intern (format "checksum-%s"     cache-id)))
           (instructions-key (intern (format "instructions-%s" cache-id)))
           (store-place      unicode-fonts-use-persistent-storage)
           (old-checksum     nil)
           (new-checksum     nil))
      (when regenerate
        (persistent-softest-store checksum-key
                                  nil
                                  store-place)
        (persistent-softest-store instructions-key
                                  nil
                                  store-place)
        (persistent-softest-flush store-place))

      (setq old-checksum (persistent-softest-fetch checksum-key
                                                   store-place))
      (setq new-checksum (unicode-fonts--configuration-checksum))

      (unless (cdr (assoc fontset-name unicode-fonts--instructions))
        (when (equal old-checksum new-checksum)
          (setq unicode-fonts--instructions
                (delq (assoc fontset-name unicode-fonts--instructions) unicode-fonts--instructions))
          (push (persistent-softest-fetch instructions-key
                                          store-place)
                unicode-fonts--instructions)))

      (unless (cdr (assoc fontset-name unicode-fonts--instructions))
        (unicode-fonts--generate-instructions fontset-name))

      (when (and store-place
                 (cdr (assoc fontset-name unicode-fonts--instructions))
                 (or regenerate
                     (not (equal old-checksum new-checksum))))
        (persistent-softest-store checksum-key
                                  new-checksum
                                  store-place)
        (let ((persistent-soft-inhibit-sanity-checks t))
          (persistent-softest-store instructions-key
                                    (assoc fontset-name unicode-fonts--instructions)
                                    store-place))
        (persistent-softest-flush store-place)))))

(defun unicode-fonts--setup-1 (fontset-name &optional regenerate)
  "Code evaluation driver for `unicode-fonts-setup'.

FONTSET-NAME is a fontset to modify using `set-fontset-font'.

Optional REGENERATE requests that the disk cache be invalidated
and regenerated."
  (when (display-multi-font-p)
    (unicode-fonts--load-or-generate-instructions fontset-name regenerate)
    (eval
     (append
      '(progn) (cdr (assoc fontset-name unicode-fonts--instructions))))
    ;; clean up the evaluated code, as it may be very large
    (setq unicode-fonts--instructions
          (delq (assoc fontset-name unicode-fonts--instructions) unicode-fonts--instructions))))

;;; main entry point

;;;###autoload
(defun unicode-fonts-setup (&optional fontset-names regenerate)
  "Set up Unicode fonts for FONTSET-NAMES.

Optional FONTSET-NAMES must be a list of strings.  Fontset names
which do not currently exist will be ignored.  The default value
is `unicode-fonts-fontset-names'.

Optional REGENERATE requests that the disk cache be invalidated
and regenerated."
  (interactive)
  (unicode-fonts-compute-skipped-fonts)
  (callf or fontset-names unicode-fonts-fontset-names)
  (dolist (fontset-name (remove-if-not #'(lambda (fs) (ignore-errors (fontset-info fs))) fontset-names))
    ;; Cocoa Emacs often crashes if this is run more than once for a fontset
    (unless (member fontset-name unicode-fonts-setup-done)
      (push fontset-name unicode-fonts-setup-done)
      (if (and (memq window-system '(ns))
               (not after-init-time))
          ;; Cocoa Emacs crashes unless this is deferred.  set-language-environment-hook
          ;; seems more logical than after-init-hook, but s-l-h appears to have already happened.
          (add-hook 'after-init-hook `(lambda () (unicode-fonts--setup-1 ,fontset-name)))
        (unicode-fonts--setup-1 fontset-name regenerate)))))

(provide 'unicode-fonts)

;;
;; Emacs
;;
;; Local Variables:
;; indent-tabs-mode: nil
;; mangle-whitespace: t
;; require-final-newline: t
;; coding: utf-8
;; byte-compile-warnings: (not cl-functions redefine)
;; End:
;;
;; LocalWords: cleartype Consolas Ethiopic Samyak BabelStone Symbola
;; LocalWords: Quivira DejaVu UnicodeFonts utils Unifont charset Cham
;; LocalWords: Akkadian Analecta Musica Doulos WenQuanYi Saysettha
;; LocalWords: Eeyek Akshar Mukti Kedage AnmolUni Padauk Lanna Banna
;; LocalWords: Koodak Ahuramzda Abyssinica Estrangelo Nisibin fontset
;; LocalWords: Wawati Weibei Alchemical Avestan Bamum Batak Bopomofo
;; LocalWords: Brahmi Buginese Buhid Carian Chakma Deseret Gurmukhi
;; LocalWords: Halfwidth Fullwidth Jamo Hanunoo Parthian Kaithi Kana
;; LocalWords: Kanbun Kangxi Katakana Kharoshthi Lepcha Limbu Mandaic
;; LocalWords: Miao Chiki Osmanya Phags Rejang Rumi Saurashtra Tham
;; LocalWords: Sharada Tagbanwa Jing Xuan Takri Thaana Yijing Damase
;; LocalWords: Gentium Batang Hana Nuosu Daicing Xiaokai Jomolhari
;; LocalWords: Alif Andale callf incf Kayah Lisu Sora Sompeng Syloti
;; LocalWords: Nagri Syllabics Arial glyphs Meetei Mayek Naskh Tahoma
;; LocalWords: ClearType Mshtakan Sylfaen Cambria Lucida Grande Yogh
;; LocalWords: Projective Sheqel Noto Nilus Namdhinggo Yunghkio Amiri
;; LocalWords: TharLon Ribeng Adinatha FreeMono FreeFont Cardo Dukor
;; LocalWords: Junicode Antinoou Bassa Epact Duployan Elbasan Grantha
;; LocalWords: Khojki Khudawadi Mahajani Manichaean Mende Kikakui
;; LocalWords: Modi Nabataean Permic Pahawh Palmyrene Siddham Tirhuta
;; LocalWords: Warang Citi Estrangela Diwani Nastaleeq Kufic Everson
;; LocalWords: ALPHABETUM Segoe Nirmala Meiryo FreeSerif Calibri Dezh
;; LocalWords: Livre Tournois Spesmilo Manat Spirant Hwair Kiyeok
;; LocalWords: Shei Alef Hiriq Shha Iotified Izhitsa Bashkir Palochka
;; LocalWords: FONTSET Abkhasian Cada Heta Sampi Pamphylian Digamma
;; LocalWords: Fontset Hanja Kanji N'ko Mingzat XQuartz XLFD Zapf
;; LocalWords: pcache KhojkiUnicodeOT AhomUnicode Ahom OldSindhi
;; LocalWords: MuktamsiddhamG MarathiCursiveG OldHungarian devel
;; LocalWords: ConScript Alist Hatran Multani SignWriting Abadi Geeza
;; LocalWords: alist multi
;;

;;; unicode-fonts.el ends here
