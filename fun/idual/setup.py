#!/usr/bin/env python
# -*- coding: utf-8 -*-
from setuptools import setup, find_packages

setup(
    name='idualctl',
    version='0.1',
    author='Vincent Ambo',
    author_email='mail@tazj.in',
    url='https://git.tazj.in/about/fun/idual',
    packages=['idual'],
    package_dir = {'idual': ''},
    scripts = ['idual.py'],
    install_requires=['broadlink>=0.13.2'],
    include_package_data=True,
)
