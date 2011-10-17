#!/usr/bin/ruby

require 'rubygems'
require 'trollop'
require 'pp'

opts = Trollop::options do
opt :license_type,
    "The license type for the code",
    :default => "lgpl"
opt :class_names,
    "Class names to create files for",
    :type => :strings
end

Trollop::die :class_names, "must be set" if !opts[:class_names]
pp opts


def getLicense(licenseType)
    print "Getting license " + licenseType
    if licenseType == "lgpl"
return '/*
 * Copyright (C) 2011 Robin Burchell <viroteck@viroteck.net>
 *
 * This program is free software; you can redistribute it and/or modify it
 * under the terms and conditions of the GNU Lesser General Public License,
 * version 2.1, as published by the Free Software Foundation.
 *
 * This program is distributed in the hope it will be useful, but WITHOUT ANY
 * WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
 * FOR A PARTICULAR PURPOSE.  See the GNU Lesser General Public License for
 * more details.
 *
 * You should have received a copy of the GNU Lesser General Public License
 * along with this program; if not, write to the Free Software Foundation,
 * Inc., 51 Franklin St - Fifth Floor, Boston, MA 02110-1301 USA.
 */';
    elsif licenseType == "apache"
return '/*
 * Copyright (C) 2011 Robin Burchell
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */'
    end
end

opts[:class_names].each { |s|
    # do main class
    File.open(s.downcase + ".h", "w") { |f|
        f.write(getLicense(opts[:license_type]) + "

#ifndef " + s.upcase + "_H
#define " + s.upcase + "_H

// Qt
#include <QObject>
#include <QString>

class " + s + " : public QObject
{
public:
    explicit " + s + "(QObject *parent);
    virtual ~" + s + "();

private:
    class Private;
    Private *d;
};

#endif // " + s.upcase + "_H")
    }

    File.open(s.downcase + ".cpp", "w") { |f|
        f.write(getLicense(opts[:license_type]) + "

// Qt
#include <QObject>

// Us
#include \"" + s.downcase + ".h\"
#include \"" + s.downcase + "_p.h\"

" + s + "::" + s + "(QObject *parent)
     : QObject(parent)
     , d(new Private(this))
{
}

" + s + "::~" + s + "()
{
}
");
    }

    # private class
    File.open(s.downcase + "_p.h", "w") { |f|
        f.write(getLicense(opts[:license_type]) + "

#ifndef " + s.upcase + "_P_H
#define " + s.upcase + "_P_H

// Qt
#include <QObject>
#include <QString>

// Us
#include \"" + s.downcase + ".h\"

class " + s + "::Private : public QObject
{
public:
    explicit Private(QObject *parent);
    virtual ~Private();
};

#endif // " + s.upcase + "_H")
    }

    File.open(s.downcase + "_p.cpp", "w") { |f|
        f.write(getLicense(opts[:license_type]) + "

#include \"" + s.downcase + ".h\"
#include \"" + s.downcase + "_p.h\"

" + s + "::Private::Private(QObject *parent)
     : QObject(parent)
{
}

" + s + "::Private::~Private()
{
}
");
    }
}
