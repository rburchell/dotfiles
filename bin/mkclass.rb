#!/usr/bin/ruby

ARGV.each { |s|
    # do main class
    File.open(s.downcase + ".h", "w") { |f|
        f.write("/*
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
 */

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
        f.write("/*
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
 */

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
        f.write("/*
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
 */

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
        f.write("/*
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
 */

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
