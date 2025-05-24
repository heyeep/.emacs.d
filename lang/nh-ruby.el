;;; nh-ruby.el --- Ruby language configuration -*- lexical-binding: t; -*-

;;; Commentary:
;; Configuration for Ruby development.

;;; Code:

;; Ruby Mode: Major mode for editing Ruby files (built-in)
;; https://www.gnu.org/software/emacs/manual/html_node/emacs/Ruby-Mode.html
(use-package ruby-mode
  :ensure t
  :mode (("\\.rb\\'" . ruby-mode)
         ("Rakefile\\'" . ruby-mode)
         ("\\.rake\\'" . ruby-mode)
         ("\\.gemspec\\'" . ruby-mode)
         ("\\.ru\\'" . ruby-mode)
         ("\\.cap\\'" . ruby-mode)
         ("\\.thor\\'" . ruby-mode)
         ("\\.jbuilder\\'" . ruby-mode)
         ("\\.prawn\\'" . ruby-mode)
         ("\\.builder\\'" . ruby-mode)
         ("\\.rabl\\'" . ruby-mode)
         ("\\.rjs\\'" . ruby-mode)
         ("\\.rxml\\'" . ruby-mode)
         ("Gemfile\\'" . ruby-mode)
         ("Guardfile\\'" . ruby-mode)
         ("Capfile\\'" . ruby-mode)
         ("Vagrantfile\\'" . ruby-mode)
         ("Berksfile\\'" . ruby-mode)
         ("Cheffile\\'" . ruby-mode)
         ("Fastfile\\'" . ruby-mode)
         ("Appraisals\\'" . ruby-mode)
         ("\\.podspec\\'" . ruby-mode))
  :interpreter ("ruby" . ruby-mode)
  :config
  ;; Do not insert encoding magic comment in new Ruby files
  (setq ruby-insert-encoding-magic-comment nil))

;; Enhanced Ruby Mode: More features for Ruby editing
;; https://github.com/zenspider/enhanced-ruby-mode
(use-package enh-ruby-mode
  :ensure t
  :mode (("\\.rb\\'" . enh-ruby-mode)
         ("Rakefile\\'" . enh-ruby-mode)
         ("\\.rake\\'" . enh-ruby-mode)
         ("\\.gemspec\\'" . enh-ruby-mode)
         ("\\.ru\\'" . enh-ruby-mode)
         ("\\.cap\\'" . enh-ruby-mode)
         ("\\.thor\\'" . enh-ruby-mode)
         ("\\.jbuilder\\'" . enh-ruby-mode)
         ("\\.prawn\\'" . enh-ruby-mode)
         ("\\.builder\\'" . enh-ruby-mode)
         ("\\.rabl\\'" . enh-ruby-mode)
         ("\\.rjs\\'" . enh-ruby-mode)
         ("\\.rxml\\'" . enh-ruby-mode)
         ("Gemfile\\'" . enh-ruby-mode)
         ("Guardfile\\'" . enh-ruby-mode)
         ("Capfile\\'" . enh-ruby-mode)
         ("Vagrantfile\\'" . enh-ruby-mode)
         ("Berksfile\\'" . enh-ruby-mode)
         ("Cheffile\\'" . enh-ruby-mode)
         ("Fastfile\\'" . enh-ruby-mode)
         ("Appraisals\\'" . enh-ruby-mode)
         ("\\.podspec\\'" . enh-ruby-mode))
  :interpreter ("ruby" . enh-ruby-mode))

;; Robe: IDE-like code navigation and documentation for Ruby
;; https://github.com/dgutov/robe
(use-package robe
  :ensure t
  :hook ((ruby-mode enh-ruby-mode) . robe-mode))

(provide 'nh-ruby)
;;; nh-ruby.el ends here 