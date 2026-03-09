---
name: "Bug Report"
about: Report a reproducible issue with swagger-codegen
title: "[Bug]: "
labels: Bug
assignees: ''
---

## Description

<!--
Clearly describe the problem you're experiencing.

Consider including:
- What client/server/language are you generating?
- What is the unexpected behavior?
- What should have happened instead?
-->

## Swagger Codegen Version

<!-- e.g. 3.0.45 -->

## Language / Generator

<!--
Specify which generator you're using.
Examples:
- java
- python
- spring
- typescript-angular
- csharp-netcore
-->

## OpenAPI/Swagger Spec

<!--
Include a minimal example of the spec that reproduces the issue.
Paste it inline (in triple backticks), or upload it as a gist.
-->

```yaml
# example snippet or URL to full spec
```
## Command Line Used

<!--
Paste the full CLI command (or Maven/Gradle config) used to run codegen.
-->

```bash
swagger-codegen generate -i petstore.yaml -l java -o output-folder
```

## Steps to Reproduce

<!--
Provide a clear, step-by-step list.

1. ...
2. ...
3. ...
-->

## Expected Behavior

<!--
Describe what should have happened.
-->

## Actual Behavior

<!--
What actually happened? Include any errors, warnings, or incorrect output.
-->

## Related Issues / Repos

<!--
If this issue seems related to an existing one, mention it here.
e.g. "Seems similar to #1234"
-->

## Environment

- OS: <!-- e.g. Ubuntu 22.04, macOS 14, Windows 11 -->
- Java Version: <!-- e.g. OpenJDK 17 -->
- Build Tool: <!-- e.g. CLI, Maven, Gradle -->
- Swagger Codegen CLI version: 

## Additional Context

<!--
Include any links, files, screenshots, or notes that help clarify the problem.
-->

## Checklist

- [ ] I have searched the [existing issues](https://github.com/swagger-api/swagger-codegen/issues) to make sure this is not a duplicate.
- [ ] I have included a minimal and reproducible spec example.
- [ ] I have explained how to reproduce the issue.
- [ ] I have specified which generator/language is affected.