import ftplib
import os

#
# Python code to send built package to win-builder
#

# Move down into the package parent directory
os.chdir('../../')

# Get the package version
desc = open('safedata/DESCRIPTION').readlines()
version = [x for x in desc if x.startswith('Version')]
version = version[0].split()[1]
package = 'safedata_{}.tar.gz'.format(version)

print('Connecting to win-builder')
c = ftplib.FTP('win-builder.r-project.org')
c.login()
c.set_pasv(False)

print('Uploading to {} release'.format(package))
c.cwd('r-release')

with open(package, 'rb') as f:
     c.storbinary('STOR ' + package, f)

print('Uploading to {} r-devel'.format(package))
c.cwd('../r-devel')

with open(package, 'rb') as f:
     c.storbinary('STOR ' + package, f)

print('Done')
c.quit()
