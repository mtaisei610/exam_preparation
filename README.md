# exam_preparation

## URL

[https://mtaisei610.github.io/exam_preparation/](https://mtaisei610.github.io/exam_preparation/)

## 科目の追加

このリポジトリをforkして、以下の手順で新しい科目を追加し、プルリクエストを送ってください。

### 1. ディレクトリの作成

`exam_preparation`リポジトリのルートディレクトリに、科目名のディレクトリを作成します。

```bash
mkdir hoge_subject
```

### 2. ファイルの追加

作成したディレクトリに、試験対策用のファイルを追加します。ファイル形式はPDF、HTMLなど、適切な形式で保存してください。

AIに資料を読ませてまとめやフラッシュカードなどを作らせ、単一のHTMLコードとして出力させるが便利です。

Markdownはレンダリングされないので、HTMLに変換してから追加してください。

著作権に注意してください。 先生の資料を直接アップロードするのは困ります。

```bash
cd hoge_subject

# 例: PDFファイルの追加
cp /path/to/your/file.pdf .
```

### 3. `index.html`の編集

#### リポジトリのルートのHTML

リポジトリのルートにある`index.html`ファイルを編集して、新しい科目のディレクトリへのリンクを追加します。

`index.html`の`<tbody>`タグ内の一番最後に以下のコードを自分の作った科目に合わせて編集してから追加してください。

ディレクトリへのパスは相対パスで指定してください。

```html
<tr>
  <td class="link">
    <a href="./あなたが作成したディレクトリ名"> 科目の説明(リンクの名前) </a>
  </td>
  <td class="size">学年と科目名</td>
  <td class="date">2026-Jan-18(←作成日を記入)</td>
</tr>
```

#### 各科目ディレクトリ内のHTML

自分が作った科目のディレクトリ内にも`index.html`を作成して、ディレクトリ内のファイルへのリンクを追加します。

リポジトリのルートにある`template.html`を`index.html`というファイル名で作成したディレクトリにコピーして編集してください。

1. `<tbody>`タグ内に、下に示した`<tr>`タグのコードを編集して挿入してください。
2. `<title>`タグの中身を科目名に変更してください。
3. `<h1>`タグの中身を科目名に変更してください。

ファイルへのパスは相対パスで指定してください。

```bash
cp ../template.html ./index.html
vim index.html
```

```html
<tr>
  <td class="link">
    <a href="./あなたが作成したファイル名">
      あなたが作成したファイル名または短かい説明(リンクの名前)
    </a>
  </td>
  <td class="size">学年と科目名</td>
  <td class="date">2026-Jan-18(←作成日を記入)</td>
</tr>
```
