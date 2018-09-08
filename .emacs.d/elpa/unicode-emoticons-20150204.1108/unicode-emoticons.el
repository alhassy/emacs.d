;;; unicode-emoticons.el --- Shortcuts for common unicode emoticons

;; Copyright (C) 2015 Gunther Hagleitner

;; Author: Gunther Hagleitner
;; Version: 0.1
;; Package-Version: 20150204.1108
;; Keywords: games, entertainment, comms
;; URL: https://github.com/hagleitn/unicode-emoticons
;; Package-Requires: ()

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:
;;
;; Simple minor mode that let's you quickly type common unicode
;; emoticons. Emoticons are stored as abbrev definitions. To enable:
;; M-x unicode-emoticons-mode <RET>
;; M-x abbrev-mode <RET>
;;
;; After that you can type "shrug0" which will expand to:
;; ¯\_(ツ)_/¯
;;
;; In order to see the definitions: M-x list-abbrevs <RET>
;;
;;; Code:

(define-abbrev-table 'unicode-emoticons-mode-abbrev-table
  '(("potter0" "(̶◉͛‿◉̶)")
    ("smile0" "(◕‿◕)")
    ("love0" "♥‿♥")
    ("umbrella0" "☂")
    ("snow0" "❅")
    ("heart0" "❤")
    ("flower0" "⚘")
    ("snowman0" "☃")
    ("yay0" "٩(⁎❛ᴗ❛⁎)۶")
    ("raiseyourdongers0" "ヽ༼ຈل͜ຈ༽ﾉ")
    ("fivedollars0" "[̲̅$̲̅(̲̅5̲̅)̲̅$̲̅]")
    ("orly0" "﴾͡๏̯͡๏﴿ O'RLY?")
    ("awkward0" "(´°ω°`)")
    ("nyan0" "~=[,,_,,]:3")
    ("shrug0" "¯\\_(ツ)_/¯")
    ("rockon0" "\\,,/(^_^)\\,,/")
    ("ctulu0" "^(;,;)^")
    ("surprised0" "ﾉﾟ0ﾟ)ﾉ")
    ("whaa0" "༼⁰o⁰；༽")
    ("meh0" "ヽ║ ˘ _ ˘ ║ノ")
    ("excited0" "[ ⇀ ‿ ↼ ]")
    ("flex0" "ᕦ( ᴼ ڡ ᴼ )ᕤ")
    ("thumbsup0" "d–(^ ‿ ^ )z")
    ("smiley0" "(◕ل͜◕)")
    ("magic0" "(つ◕౪◕)つ━☆ﾟ.*･｡ﾟ")
    ("cool0" "┌(▀Ĺ̯▀)┐")
    ("cry0" "ヽ༼ಢ_ಢ༽ﾉ☂")
    ("dance0" "┏(-_-)┓┏(-_-)┛┗(-_-﻿ )┓")
    ("dead0" "(✖╭╮✖)")
    ("cute0" "(◕ᴥ◕)")
    ("angry0" "ლ(ಠ益ಠ)ლ")
    ("confused0" "「(°ヘ°)")
    ("hatersgonnahate0" "ᕕ( ᐛ )ᕗ")
    ("tears0" "༼ ༎ຶ ෴ ༎ຶ༽")
    ("fliptable0" "(╯°□°）╯︵ ┻━┻")
    ("gimme0" "༼ つ ◕_◕ ༽つ")
    ("happy0" "(✿◠‿◠)")
    ("confusedshrug0" "¯\\(°_o)/¯")
    ("disapprove0" "(ಠ_ಠ)")
    ("sad0" "(╯_╰)")
    ("pleased0" "(─‿‿─)")
    ("shifty0" "(¬､¬)")
    ("pingpong0" "( •_•)O*¯`·.¸.·´¯`°Q(•_• )")
    ("yesss0" "(งಠ_ಠ)ง")
    ("yuno0" "щ(゜ロ゜щ)")
    ("facepalm0" "(－‸ლ)")
    ("doublefacepalm0" "(ლ‸－)(－‸ლ)")
    ("pointout0" "( ՞ਊ ՞)☝")
    ("success0" "(•̀ᴗ•́)و ̑̑")
    ("csi0" "•_•)
( •_•)>⌐■-■
(⌐■_■)
")))

(add-to-list 'abbrev-minor-mode-table-alist
	     `(unicode-emoticons-mode ,unicode-emoticons-mode-abbrev-table))

;;;###autoload
(define-minor-mode unicode-emoticons-mode
  "Unicode-emoticons-mode enables expansion of certain keywords to
unicode emoticons. It is built as an abbrev table, and therefore also
needs the abbrev-mode to be enabled for it to take affect."
  :group 'unicode-emoticons
  :init-value nil)

(provide 'unicode-emoticons)
;;; unicode-emoticons.el ends here
