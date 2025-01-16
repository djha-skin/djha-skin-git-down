# git-down

## Design Document

We use a hierarchical approach to configuration, in the usual way.
I want to make a code downloader that cuts out the middle man as much as
possible. 9/10 I just want the code on my laptop.

In order to download code from git as painlessly as possible, I need to know
what subset of files to grab, an acceptable range of commits from which to grab
them, and what that code's dependencies are.

Some mechanism must tell me the dependencies. Actually, everything.

This is a build downloader tool. It's for builds.

registry, builder, package, version -> git URI, subfolder, sha

Now that's what we need. I, as the git downloader, have some knowledge about
shas. I can be in charge of selecting a good sha. But I need some basic
information around default branch and URI.

So I need ecosystem, builder, package, version-spec -> git URI, subfolder, branch, tag-converter

These can be simply loaded into the config file in the usual way, with one
config file for the ecosystem, one for the builder and one config file for the
package.

The config file can be edited via pull request and simply served raw via github.

That's one way to do it -- curation.

The other ways are more automated, like groveling.

Do I want to introduce that degree of freedom.


OK but we haven't defined the separation, the API yet. Let's get that down.


I have a git repository. That's how this whole thing starts.

I need to download other git repositories. I need to know what those are.

Presumably a build tool could tell me what packages it needs, and any versioning
information "it wants to give me".


```lisp
(defstruct package-dependency name git-url versionspec version git-version)
  
(defgeneric package-dependencies (repository)
  "
  Inspects the @param(repository) (a path) for dependencies and any version
  information.
  "
  )
```

I would do different things depending on what versioning Information I got:

1. If I got a git URL and ref, "just use that"
2. If I got a version spec and package, I need to resolve those then pick a ref
3. If I got a version and package, I need to resolve those

Now those 3 things are very different. What happens when one tool picks one ref
and another repo picks another? Well, those 3 are very different. They basically
boil down to version specs that are VERY specific.

Any of those three, no matter what they are, need first to be resolved to a git
URL and collection of commits. This is the basic unit of dependency resolution,
the dependency.

So the above code changes:

```lisp
;; cache folder is a configuration item
;; 

(defstruct git-dependency uri commits)
  
(defgeneric git-dependencies (repository &optional checkout)
  "
  Inspects the @param(repository) (a path) for dependencies and any version
  information.

  Uses @param(checkout) to checkout the repo to the cache folder.

  (checkout is usually the same but present for dependency injection reasons.)

  Returns a git-dependency.
  "
  )
```

There's two concerns in the above function we must tease apart.

One is inspecting a repository to find out which "packages" are present, and
_whether the packages present will meet requirements_ (no faff about versioning,
_since we don't even know if the build tool uses versioning_). The other is
checking out different refs and letting the inspector make this check. This can be done
either by inspecting a version number and comparing with a version spec OR by
running unit tests, etc. At our disposal is contents from the requiring repo and
contents from the depended one.

This process of inspection MAY check out different versions of the dependency,
but MUST NOT make permanent changes to the files OR commit anything. It can
yield a "usable" commit in time, but it must yield many usable commits,
hopefully, to avoid causing conflicts with others. The tool might use git bisect
for this, and just give back a range of commits.

This is why versioning is so popular. You know what's cheap? checking a version
spec against a bunch of tags. Maybe we spot the developer this -- we provide a
way to check specs against tags. However, even in this case, the tag is usually
a project-specific thing. The only thing that truly "matters" is the version
told to the build tool, e.g. maven. So this process of paging through and
checking out is needed anyway.

However.

We can do the clean architecture thing here as well by abstracting the I/O of
checkouts. The build tool can tell us what file's contents it needs to inspect
(there is usually only one of these, but I guess the tool might provide more.)
Still, I think we don't hide the I/O here, at least the reading. What we _could_
do is handle the checkouts, then have the dependency check inspect the two
folders. We just run a git clean before/after and hope they didn't make any
commits.

But we wouldn't know what to check out, and the implied necessity of unit tests
is a heavy one to impose. But static checks could be run, etc. This coincides
with Hickey's vision of, why do we need version numbers.  Maybe he was a CL man
after all.

I think we do that. We handle checkouts, have to build tool inspect. We can
configure things like e.g. `prefer-tags` or `enforce-tags` to ensure that only
tags are checked out, or allow things like `git bisect` on an entire branch.

One thing, though: The cheaper the check, the better. Static analysis, _maybe_ a
quick compilation check/smoke test, and we're done after that.

So to facilitate the compilation check, the package MAY be installed, but MUST
be uninstalled after the check and MUST NOT clobber other files from other
packages.

This is getting exciting.

Boom. Versionless versioning.

OK so the tool leaves through the repo and figures out what commits are OK
(with our help).

If two version specs cannot
agree on a subset of commits, they are handed over to any configured conflict
resolution strategy. Configured, or build tool-specific? Solve this by putting
build tool into the configuration file chain. So configured.

```lisp
(defconfig conflict-resolution
  :latest
  :least
  :a
  :b)
```

OK so I now have commits and a git repo for each dependency. We check them all
out to the latest git commit within those allowable bands and recurse.

We must be very careful here. Are we broken diamond or not? We must think hard
about this.
