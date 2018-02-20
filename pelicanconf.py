#!/usr/bin/env python
# -*- coding: utf-8 -*- #
from __future__ import unicode_literals

AUTHOR = u'Aditya Mangal'
SITENAME = u"Aditya Mangal's Blog"
SITEURL = 'http://adityamangal.com'
SITETITLE = AUTHOR
SITESUBTITLE = 'Software Engineer, Data Science Enthusiast'
SITEDESCRIPTION = '%s\'s Thoughts and Writings' % AUTHOR
SITELOGO = '/images/profile.png'
FAVICON = '/images/favicon.png'
BROWSER_COLOR = '#333333'
#PYGMENTS_STYLE = 'monokai'

ROBOTS = 'index, follow'

TIMEZONE = 'America/Los_Angeles'

I18N_TEMPLATES_LANG = 'en'
DEFAULT_LANG = 'en'
OG_LOCALE = 'en_US'
LOCALE = 'en_US'

DATE_FORMATS = {
    'en': '%B %d, %Y',
}

PATH = 'content'
STATIC_PATHS = ['images', 'figure']
RMD_READER_RENAME_PLOT = 'directory'
RMD_READER_KNITR_OPTS_CHUNK = {'fig.path':  'figure/', 'echo': 'TRUE'}

# Feed generation is usually not desired when developing
FEED_ALL_ATOM = None
CATEGORY_FEED_ATOM = None
TRANSLATION_FEED_ATOM = None
AUTHOR_FEED_ATOM = None
AUTHOR_FEED_RSS = None

# Blogroll
LINKS = (('Portfolio', 'http://adityamangal.com'),
        )

# Social widget
SOCIAL = (('github', 'https://github.com/adityamangal410'),
          ('linkedin', 'https://www.linkedin.com/in/adityamangal410/'),)

USE_FOLDER_AS_CATEGORY = False
MAIN_MENU = True
HOME_HIDE_TAGS = True

MENUITEMS = (('Archives', '/archives.html'),
             ('Categories', '/categories.html'),
             ('Tags', '/tags.html'),)

DEFAULT_PAGINATION = 8

# Uncomment following line if you want document-relative URLs when developing
#RELATIVE_URLS = True

THEME = 'Flex'

PLUGIN_PATHS = ['/Users/amangal/pelican-plugins/']
PLUGINS = ['sitemap', 'rmd_reader']

SITEMAP = {
    'format': 'xml',
    'priorities': {
        'articles': 0.6,
        'indexes': 0.6,
        'pages': 0.5,
    },
    'changefreqs': {
        'articles': 'monthly',
        'indexes': 'daily',
        'pages': 'monthly',
    }
}

ADD_THIS_ID = 'ra-59cc3b6a1ce7db70'
GOOGLE_ANALYTICS = 'UA-78062737-2'
