require 'json'
  infile = 'exports/extracted.json'
  abort("File #{infile} not readable") unless File.readable? infile
  data = JSON.load_file(infile)
  data.select! { |e| e.keys.any? { |k| k =~ /^P_/ } }
  data.map! do |h|
    h.reduce(Hash.new) do |paper_info, (prop, v)|
      prop_formatted = prop.downcase.delete_prefix('p_')

      unless ['devices', 'priority', 'item'].include? prop_formatted
        v = 'both LB and NLB' if prop_formatted == 'fw_types' && v == 'linux non-linux'
        v = "\\citeauthor{#{v.delete_prefix('papers/').delete_suffix('.pdf')}} \\cite{#{v.delete_prefix('papers/').delete_suffix('.pdf')}}" if prop_formatted == 'noter_document'
        v = v.split.map { |arch| arch =~ /ARM/ ? 'ARM' : arch }.uniq.join(' ') if prop_formatted == 'isa'
        paper_info[prop_formatted] = v
      end

      paper_info
    end
  end
  data.sort_by! { -(_1['n_analysed'].to_i) }

  cols = [['noter_document', 'Document'],
          ['devices_sector', 'Sector'],
          ['dataset', 'Dataset status'],
          ['isa', 'ISAs'],
          ['n_analysed', '\\# Analyzed'],
          ['n_scraped', '\\# Scraped'],
          ['whats_analysed', 'Analysis focus'],
          ['scraping_approaches', 'Scraping approaches'],
          ['analyses', 'Analysis types']]


  preamble = ->(fwtype) {
    <<~EOF
           \\begin{table*}[]
           \\resizebox{\\linewidth}{!}{%
           \\begin{tabular}{@{} l l l l r r l l l @{}}
           \\toprule
           #{cols.map(&:last).flatten.map { |s| "\\textbf{#{s}}" }.join(" & ")} \\\\
     EOF
  }

  postamble = ->(fwtype) {
    <<~EOF
    \\bottomrule
         \\end{tabular}%
}
             \\caption{Categorisation of past research analyzing #{fwtype} firmware.}
           \\label{tab:bigtable-#{fwtype}}
\\end{table*}
    EOF
  }

  ['non-Linux', 'Linux', 'both LB and NLB'].each do |fwtype|
    result = data.select {|d| d['fw_types'].downcase == fwtype.downcase }.reduce(preamble[fwtype]) do |result, h|
      <<~EOF
      #{result}
      \\midrule
      #{cols.map(&:first).flatten.map { |c| h[c].nil? ? '-' : h[c].split(' ').map { |x| x.gsub(/-/, ' ') }.join(', ') }.join(" & ")} \\\\
      EOF
    end + postamble[fwtype]
    File.write("exports/tables/bigtable-#{fwtype}.tex", result)
  end

require 'json'
infile = 'exports/extracted.json'
abort("File #{infile} not readable") unless File.readable? infile

data = JSON.load_file(infile)
data.select! { |e| e.keys.any? { |k| k =~ /^P_/ } }
data.map! do |d|
  h = d.reduce(Hash.new) do |paper_info, (prop, v)|
    prop_formatted = prop.downcase.delete_prefix('p_')
    case prop_formatted
    # numeric properties
    when 'n_scraped', 'n_analysed', 'priority', 'pct_nlb', 'pct_lb'
      paper_info[prop_formatted] = (Integer(v) rescue -1)
    when 'noter_document' # skip
      paper_info
    when 'item'
      paper_info[prop_formatted] = v
    when 'devices'
      paper_info[prop_formatted] = v.gsub('"', '').split(', ')
    else
      paper_info[prop_formatted] = v.split
    end
    paper_info
  end
  h['n_scraped'] = -1 unless h['n_scraped']
  h['n_analysed'] = -1 unless h['n_analysed']
  h['pct_lb'] = -1 unless h['pct_lb']
  h['pct_nlb'] = -1 unless h['pct_nlb']
  h
end

tsv_header = data.map(&:keys).flatten.uniq

tsv_lines = data.reduce([tsv_header.join("\t")]) do |finstr, d|
  s = tsv_header.reduce([]) do |arr, h|
    if d[h].is_a? Array
      arr << d[h].join(',')
    else
      arr << d[h].to_s
    end
  end
  finstr << s.join("\t")
end

File.write 'exports/corpus2.tsv', tsv_lines.join("\n")
