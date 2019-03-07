This global minor mode provides a simple way to switch between layouts and
the buffers you left open before you switched (unless you closed it).

It doesn't require any setup at all more than:
(0blayout-mode)

When you start Emacs with 0blayout loaded, you will have a default layout
named "default", and then you can create new layouts (<prefix> C-c), switch
layouts (<prefix> C-b), and kill the current layout (<prefix> C-k).
The default <prefix> is (C-c C-l), but you can change it using:
(0blayout-add-keybindings-with-prefix "<your prefix>")

You can also customize-variable to change the name of the default session.

The project is hosted at https://github.com/etu/0blayout
There you can leave bug-reports and suggestions.

Another comparable mode is eyebrowse which have been developed for longer.
