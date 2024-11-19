# SRT Tools

This application is intended to eventually provide a suite of small utilities
for working with [SubRip](https://en.wikipedia.org/wiki/SubRip) format subtitles
(.srt files).

Right now it offers a single tool, `compress`, that will cleanup repeated
subtitles.

## Quickstart

The easiest way to run this application is with [nix](https://nixos.org/). For
example, if you want to compress `in.srt` you can run:

```
nix run github:rebeccaskinner/srt-tools in.srt out.srt
```

This will run the compress tool on `in.srt` to generate a new file, `out.srt`.

## The Compress Tool

Text-to-speech models like whisper can sometimes generate subtitles that contain
long runs of repeated subtitles, especially during periods of silence or when
there are background sounds playing.

Having so much repetition can make it difficult to manually edit subtitles if
you want to translate them into another language, add formatting, or fix up
incorrectly or inconsistently spelled names.

The compress tool replaces runs of repeated subtitles with a single subtitle
that covers the duration of the repetition. For example, here's a subtitle
generated from a home movie using [whisper.cpp](https://github.com/ggerganov/whisper.cpp):


```
1
00:00:00,000 --> 00:00:09,640
 I have this, but it doesn't have a very good cut.

2
00:00:09,640 --> 00:00:11,080
 >> Oh, that would be perfect.

3
00:00:11,080 --> 00:00:11,580
 Thank you.

4
00:00:11,580 --> 00:00:14,600
 [ Background noise ]

5
00:00:14,600 --> 00:00:37,600
 [ Background noise ]

6
00:00:37,600 --> 00:00:39,400
 [ Background noise ]

7
00:00:39,400 --> 00:00:40,600
 [ Background noise ]

8
00:00:40,600 --> 00:00:46,600
 [ Background noise ]
```

You'll notice that we have five reptitions of the `[ Background noise ]`
subtitle. After running compress we'll get a new file where these have been
combined:

```
1
00:00:00,000 --> 00:00:09,640
 I have this, but it doesn't have a very good cut.

2
00:00:09,640 --> 00:00:11,080
 >> Oh, that would be perfect.

3
00:00:11,080 --> 00:00:11,580
 Thank you.

4
00:00:11,580 --> 00:00:46,600
 [ Background noise ]
```
