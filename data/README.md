# working directory for saving data

Avoid uploading datasets saved to git repo as they are large and slow things down.

If you have a dataset you want to save that is <=50.0 MB move it to either the data.beta or data.stable folders. All other files in models.tmp will be ignored by .gitignore file.

Example:
```bash
remote: warning: File log/development.log is 98.59 MB; this is larger than GitHub\'s recommended maximum file size of 50.00 MB
remote: error: GH001: Large files detected. You may want to try Git Large File Storage - https://git-lfs.github.com.
```
