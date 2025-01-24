# git-down

## Philosophy

So you want dependencies installed for a certain project.

_Why not use <ECOSYSTEM> package manager?_

Ecosystem package managers don't always have the features I want surrounding
build reproducibility. Also, maybe it's because it's not just <ECOSYSTEM>
packages. I consistently want these things:

1. Reproducibility: I can get up and walk away from the project and come back to
   it and the build still works.

2. Provenance: I want to know what's in the final built product.


_Why not use vendoring then?_

Vendoring solves #1 and #2, but has its annoyances. Now when I update
dependencies my PR diff explodes. It is difficult to tell from the diff what
_I_ changed (the dependency versions) rather than what _upstream_ changed.

Also,

3. I don't want to have to think about transitive dependencies until the build
   works. At that point, then I'd like to be able to take a look at what's
   downloaded and see if I like it, but if I don't want to think about it I
   shouldn't need to do so.

_Why not use nix then?_

Interesting question. Why _not_ use NIX?

NIX is a great tool for reproducibility, and solves #1-3, but it's a lot to take
on to get into its ecosystem. Now the user must install nix and learn to use
this new tool (and hope it won't break the user's current setup). I wanted
something much simpler to use, and much more transparent.

4. Tool must be _dead simple_ to use and install, and must _work with other
   established build tools_ by default. It must behave as if I myself did all
   the downloading work.

_Why not use docker?_

Docker is a great tool for reproducibility, but then you have to run a docker
image to use a tool. This seems silly for small stuff.

5. Tool must allow build tools to create "normal", easily-distributed artifacts,
   such as the build tool normally produces -- JARs for maven, executables for
   Lisp, `.so`'s for gcc, etc.

_Why not use buildpack?_

Buildpack is a very interesting project, one from which I will likely steal
ideas from before this is all over. However, buildpack relies on conventional
package managers, and I don't like all of those (#1). Further, it doesn't speak
very well to things that use two or more languages.


## Glossary

_bit_: A 0 or a 1.

_file_: An representation of a collection of bits.

_machine_: A physical devices which may execute code and efficiently store
files.

_runtime_: An automaton, implemented either in hardware or software, which may
execute code.

_code_: A representation of instructions to be executed by a runtime.

_text_: Code which is human-readable.

_bytecode_: Code which may be directly executed via a runtime.

_artifact_: A file which contains bytecode.

_program_: An artifact whose contained instructions may be directly executed
within a particular environment.

_compiler_: A compiler is a program that turns text into bytecode.

_text file_: A file which efficiently represents textual information.

_execution_: The process of a runtime executing bytecode, one instruction at a
time, beginning at the first included instruction.

_environment_: A collection of artifacts and ad-hoc instructions made available
to a runtime for reading and execution.

_dependency_: Either an artifact or text file which is required to make a
particular artifact.

_repository_: A collection of dependencies which are immediately useful
for creating an artifact. Repositories track the history of their files and are
primarily useful in storing text dependencies.

_build_: A process within an environment whereby files in one or more
repositories are used to create one or more artifacts. This process may involve
several compiler executions, Typically, there are more
artifacts than repositories, and usually only one repository, but at least one
repository and one artifact is involved.

_language_: Text which is all interpretable by the same compiler.

_build coordinator_: A program which is responsible for creating artifacts from
dependencies.

_dependency manager_: A program which is responsible for downloading all
necessary dependencies in order that a build may take place.

## Discussion

It follows from the above definitions that

At least one compiler is involved, and possibly several, in any given build.

The build coordinator need not be the same as the compiler and has nothing to
do with the language definition. Several different languages may use one or
more coordinators. `make` is language agnostic and used in several places;
`ant` and `mvn` are both used as build coordinators for the same language.
Sometimes a build coordinator achieves ubiquity, sometimes not, depends on
the language.

Indeed, more than one language may be present in any particular build, since
different compilers can turn different text into the bytecode runnable via
the same runtime.

Everything is fast and loose, and definitions are hard to pin down, but we've
got to do _something_.

## Design Assumptions

1. The user has all the programs in their environment necessary to run builds.

2. All builds may be strictly fed text files (no artifacts) as their
   dependencies and things will "still work".

3. All text files are to be had from git repositories.

4. The location of where the dependency text files must be located in relation
   to the environment is known.

5. The git repositories where text files are stored is _transitively_ known.

## Design

I want to make a code downloader that cuts out the middle man as much as
possible. 9 times out of 10, "I just want the code" on my laptop.

In order to download code from git as painlessly as possible, I need to know
what subset of files to grab, an acceptable range of commits from which to grab
them, and what that code's dependencies are.

Some mechanism must tell me the dependencies. Actually, everything.

This is a build downloader tool. It's for builds. So the build tool will "tell
me" stuff, but it doesn't _know_ that it will tell me stuff. This build tool is
configurable with the `makeprg` key.

That can tell me if the dependencies work together.

But, how to tell me what to grab?

Say I'm working with maven. Then maven just downloads stuff.

OK but say I'm working with Ant. "Why?"

OK fine. I don't want to use this maybe to download everything, but at least
some of the dependencies are written by my good self or my company. (This tool
doesn't address that anyway.) I want to (be able to) DOWN THEM ALL

I need some way for the "outside world" to communicate what dependencies to
download, and hopefully even a few refs if they can give them to me, or a range.

Once I have that, the query tool, or perhaps `queryprg`, then I'm golden.


```lisp
;;; Let's use s(erapeum) and f(set) for funs.
(defun resolve (make query present required)
    (if (null required)
      present
      (let ((requirement (car required))
            (rest-reqs (cdr required)))
          (multiple-value-bind (repo-name repo-uri repo-refs)
                                (query requirement)
          (let ((usable-refs
            (a:if-let ((provided-coordinate
                         (f:@ provided repo-uri)))
                  (resolve-conflict requirement repo-uri
                    (coord-ref provided-coordinate))
                repo-refs)))
            (loop for ref in usable-refs
                  for coordinate in (make-coordinate repo-name repo-uri ref)
                  do
                  ;; A LOT happens here. This checks something out to a ref,
                  ;; inspects for packages using `(inspect-for-packages)`, and
                  ;; copies those packages in to the production directory.
                  (checkout repo-name repo-uri ref)
                  ;; build tool
                  ;; inspect-for-dependencies takes `requirement` which is
                  ;; presumably a package spec. There could be multiple packages
                  ;; in one repo, so the inspector needs to know what package to
                  ;; inspect.
                  (let ((dependencies (inspect-for-dependencies repo-name requirement))
                        (implied-provided (f:with (coord-uri coordinate) coordinate provided))
                        (implied-requirements (f:concat required dependencies)))
                    
                    (multiple-value-bind
                      (status provided)
                      (resolve (make query implied-provided
                        implied-requirements))
                        (when (and (eql status :successful)
                                 (check-build repo-name))
                          (return (values :successful
                              implied-provided)))))
                  finally
                  (return :unsuccessful))))))
```

OK so the above is my sketch for the resolving program.

Let's tear that a little bit apart.

The astute reader will notice there are some "magic happens here" steps.

The goal is to make a code downloader that knows how to talk to git, git hosting
providers, and build tools. _That's it_. No tricks, no weapons. Skill against
skill alone.

So it asks the build tool for the name of a known file that contains
instructions for the tool. This file presumably has at least the name and
version number of the package, but we don't even know if the build tool uses
version numbers(!). (Some don't.)

We need to be able to show it this file and ask it, "Does this file tell you to
build requirement _x_?" If it does, and it passes some of our heuristic checks,
it gets downloaded. As an added measure of are-we-sure, there's a prompt to the
user to download it.

So there's some guessing on the part of the engine. Many git repos could resolve
as the same thing. Forks explicitly allow for this. How to tell if the
requirement is the one we want? By checking it out and seeing if things build.

The heuristics will be number of stars, issues closed in the last 90 days, etc.
It's really a human/AI complicated judgement call. But sometimes it's easy to
make. 

This is the worst thing about this idea -- guessing at what things come from
what repositories. This is the core reason why registries exist.





Looking at that, there's obviously a bunch of unhappy paths to guard, inlining,
etc. to do, but that's the basic gist of what's going on.



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
