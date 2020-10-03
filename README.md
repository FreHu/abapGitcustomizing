# abapGitcustomizing
abapGit for customizing content

# Motivation
Since more and more developers are started using [abapGit](https://docs.abapgit.org/), we have now many open source projects in ABAP. The abapGit works with TADIR objects which are created under a package. But the tool lacks the support for customizing content i.e. TABU, VDAT, CDAT, TDAT or TLOGO objects.

# Idea
Use existing customizing tool [Business Configuration Sets](https://help.sap.com/viewer/521cd184dd2f491a9a4179edb66951c3/7.52.6/en-US/4da559a1327f212de10000000a42189e.html) to handle the customizing content.

# Prerequisite
- Developer version of [abapGit](https://docs.abapgit.org/guide-install.html#install-developer-version) is installed in the system
- SAP NetWeaver 7.40 or higher

# Installation
Install the repo via abapGit.

# Transaction
ZAGC

# Supports
- Only content recorded in a customizing request
- Customizing content of type TABU, VDAT and CDAT
- Stage of selected content to a repository. Also it is possible to Pull the content i.e. customizing content is deployed to corresponding table/view/viewcluster
- Deleted content i.e. Stage/Pull of deleted content is possible

# Limitation
- Repository must be created using abapGit
- If the repository is created to only handle customizing content then no objects will be created under the package which is associated with the repo
- There are limitations related to handling of objects in BC Sets i.e. no system objects or object which has Before Export Method
