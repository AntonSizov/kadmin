function Trim(text, character)
{
	while( text.substring(0, 1) == character )
		text = text.substring(1, text.length);
		
	while( text.substring(text.length - 1, text.length) == character )
		text = text.substring(0, text.length - 1);
		
	return text;
}